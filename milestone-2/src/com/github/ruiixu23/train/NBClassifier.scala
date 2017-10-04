package com.github.ruiixu23.train

import java.io.{File, FileNotFoundException, IOException}

import com.github.ruiixu23.corpus.{CorpusReader, CorpusUtils}

import scala.collection.mutable
import scala.io.Source

/**
 * Created by ruiixu23 on 20/11/14.
 *
 * Implement the Multinomial Naive Bayes algorithm
 */
class NBClassifier() extends Classifier {
  private var numOfDocs = 0
  private var vocabularySize = 0
  private val topicTcs = new mutable.HashMap[String, Int]()
  private val logPrior = new mutable.HashMap[String, Double]()
  private val logLikelihood = new mutable.HashMap[String, mutable.HashMap[String, Double]]

  override def load(path: String, converted: Boolean): Unit = {
    val topicDistribution = new mutable.HashMap[String, Int]()
    val topicTfs = new mutable.HashMap[String, mutable.HashMap[String, Int]]()

    if (converted) {
      val directory = new File(path)
      if (!directory.exists()) throw new FileNotFoundException("The directory cannot be found at the path:" + path)
      if (!directory.isDirectory) throw new IOException("The resource located at path:" + path + " is not a directory")

      val lines = Source.fromFile(new File(directory, corpusFileName), encoding).getLines()
      while(lines.hasNext) {
        // The item id is not needed
        lines.next()
        // Load topics
        val topicsLine = lines.next()
        val topics = topicsLine.split(",").toSet
        // Load tokens
        val numOfTokens = lines.next().toInt
        val tokenGroups = new mutable.HashMap[String, Int]()
        for (i <- 0 until numOfTokens) {
          val group = lines.next().split(",", 2)
          val tf = group(0).toInt
          val token = group(1)
          tokenGroups += (token -> tf)
        }

        if (topicsLine.length > 0) {
          topics.foreach(topic => {
            // Update topic distribution
            topicDistribution += (topic -> (topicDistribution.getOrElse(topic, 0) + 1))

            var tc = 0
            tokenGroups.foreach(group => {
              val token = group._1
              val tf = group._2
              // Update the individual term frequency of the topic
              val topicTf = topicTfs.getOrElse(token, new mutable.HashMap[String, Int]())
              topicTf += (topic -> (topicTf.getOrElse(topic, 0) + tf))
              topicTfs += (token -> topicTf)

              // Update the total term frequency
              tc += tf
            })

            // Update the total term frequency of the topic
            topicTcs += (topic -> (topicTcs.getOrElse(topic, 0) + tc))
          })

          numOfDocs += 1
          if (numOfDocs % 5000 == 0) println(numOfDocs + " documents loaded")
        }
      }
    } else {
      val corpusReader = new CorpusReader(path)
      while (corpusReader.hasNext) {
        val doc = corpusReader.next()
        val topics = doc.getTopics
        if (topics.size > 0) {
          val tokens = CorpusUtils.removeStopWords(CorpusUtils.tokenizeWithLematizer(doc.getTitle + ". " + doc.getText))
          val tokenGroups = tokens.groupBy(identity).map(group => group._1 -> group._2.size)
          topics.foreach(topic => {
            // Update topic distribution
            topicDistribution += (topic -> (topicDistribution.getOrElse(topic, 0) + 1))

            var tc = 0
            tokenGroups.foreach(i => {
              val token = i._1
              val tf = i._2
              // Update the individual term frequency of the topic
              val topicTf = topicTfs.getOrElse(token, new mutable.HashMap[String, Int]())
              topicTf += (topic -> (topicTf.getOrElse(topic, 0) + tf))
              topicTfs += (token -> topicTf)

              // Update the total term frequency
              tc += tf
            })

            // Update the total term frequency of the topic
            topicTcs += (topic -> (topicTcs.getOrElse(topic, 0) + tc))
          })

          numOfDocs += 1
          if (numOfDocs % 5000 == 0) println(numOfDocs + " documents loaded")
        }
      }
    }

    vocabularySize = topicTfs.keySet.size

    // Calculate the log prior probabilities
    topicDistribution.foreach(i => logPrior += (i._1 -> math.log(i._2.toDouble / numOfDocs)))

    // Calculate the log conditional probabilities with smoothing
    topicTfs.foreach(i => {
      val token = i._1
      val topicTf = i._2
      val logTopicLikelihood = new mutable.HashMap[String, Double]()
      topicTf.foreach(j => {
        val topic = j._1
        val tf = j._2
        logTopicLikelihood += (topic -> math.log((tf + 1).toDouble / (topicTcs(topic) + vocabularySize)))
      })
      logLikelihood += (token -> logTopicLikelihood)
    })
  }

  def predict(tokenGroups: Map[String, Int], maxNumOfTopics: Int): Set[String] = {
    val queue = new mutable.PriorityQueue[TopicPair]()
    val candidateTopics = logPrior.keySet

    candidateTopics.foreach(topic => {
      // Add prior probability
      var score = logPrior(topic)

      // Add likelihood
      tokenGroups.foreach(group => {
        val token = group._1
        val tf = group._2
        if (logLikelihood.contains(token)) score += tf * logLikelihood(token).getOrElse(topic, math.log(1.0 / (topicTcs(topic) + vocabularySize)))
        else score += tf * math.log(1.0 / (topicTcs(topic) + vocabularySize))
      })

      if (queue.size < maxNumOfTopics) queue.enqueue(TopicPair(topic, score))
      else {
        val minTopicPair = queue.dequeue()
        if (minTopicPair.score < score) queue.enqueue(TopicPair(topic, score))
        else queue.enqueue(minTopicPair)
      }
    })

    val predictedTopics = new mutable.HashSet[String]()
    while (queue.nonEmpty) predictedTopics += queue.dequeue().topic
    predictedTopics.toSet
  }
}