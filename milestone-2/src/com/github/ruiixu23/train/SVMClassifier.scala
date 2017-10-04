package com.github.ruiixu23.train

import java.io.{File, FileNotFoundException, IOException}

import breeze.linalg.{SparseVector, norm}
import com.github.ruiixu23.corpus.{CorpusReader, CorpusUtils}

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

/**
 * Created by ruiixu23 on 01/12/14.
 *
 * Implement the SVM algorithm
 */
class SVMClassifier extends Classifier {
  private val targets = new mutable.HashMap[String, Set[String]]()
  private val corpus = new mutable.HashMap[String, Map[String, Int]]()
  private val dfs = new mutable.HashMap[String, Int]()
  private val cfs = new mutable.HashMap[String, Int]()
  private val topicWeights = new mutable.HashMap[String, Double]()
  private val tokenIndices = new mutable.HashMap[String, Int]()
  private var numOfDocs = 0
  private var sizeOfVector = 0
  private var thetas = new mutable.HashMap[String, SparseVector[Double]]()

  override def load(path: String, converted: Boolean): Unit = {
    val topicDistribution = new mutable.HashMap[String, Int]()

    if (converted) {
      val directory = new File(path)
      if (!directory.exists()) throw new FileNotFoundException("The directory cannot be found at the path:" + path)
      if (!directory.isDirectory) throw new IOException("The resource located at path:" + path + " is not a directory")

      val lines = Source.fromFile(new File(directory, corpusFileName), encoding).getLines()
      while(lines.hasNext) {
        // Load item id
        val itemId = lines.next()
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
          // Update topic distribution
          topics.foreach(topic => topicDistribution += (topic -> (topicDistribution.getOrElse(topic, 0) + 1)))
          // Update targets
          targets += (itemId -> topics)
          // Update corpus
          corpus += (itemId -> tokenGroups.toMap)
          // Update the document frequency & collection frequency
          tokenGroups.foreach(group => {
            val token = group._1
            val tf = group._2
            dfs += (token -> (dfs.getOrElse(token, 0) + 1))
            cfs += (token -> (cfs.getOrElse(token, 0) + tf))
          })
          // Update the number of documents
          numOfDocs += 1
          if (numOfDocs % 5000 == 0) println(numOfDocs + " documents loaded")
        }
      }
    } else {
      val corpusReader = new CorpusReader(path)
      while (corpusReader.hasNext) {
        val doc = corpusReader.next()
        val itemId = doc.getItemId
        val topics = doc.getTopics
        if (topics.size > 0) {
          val tokens = CorpusUtils.removeStopWords(CorpusUtils.tokenizeWithLematizer(doc.getTitle + ". " + doc.getText))
          val tokenGroups = tokens.groupBy(identity).map(group => group._1 -> group._2.size)
          // Update topics distribution
          topics.foreach(topic => topicDistribution += (topic -> (topicDistribution.getOrElse(topic, 0) + 1)))
          // Update targets
          targets += (itemId -> topics)
          // Update corpus
          corpus += (itemId -> tokenGroups)
          // Update the document frequency & collection frequency
          tokenGroups.foreach(group => {
            val token = group._1
            val tf = group._2
            dfs += (token -> (dfs.getOrElse(token, 0) + 1))
            cfs += (token -> (cfs.getOrElse(token, 0) + tf))
          })
          // Update the number of documents
          numOfDocs += 1
          if (numOfDocs % 5000 == 0) println(numOfDocs + " documents loaded")
        }
      }
    }

    // Calculate weight for each topic
    topicDistribution.foreach(i => topicWeights += (i._1 -> (i._2.toDouble / numOfDocs)))

    // Create mapping between token and feature space index
    cfs.toList.sortWith((x, y) => x._2 > y._2)
      .zipWithIndex.foreach(i => tokenIndices += (i._1._1 -> i._2))
  }

  def train(lambda: Double, numOfFeatures: Int, numOfIterations: Int): Unit = {
    // Calculate feature space dimension
    sizeOfVector = tokenIndices.size.min(numOfFeatures)

    // Initialize weight vectors
    val random = new Random(System.currentTimeMillis())
    val topics = topicWeights.keySet
    topics.foreach(topic => {
      val theta = SparseVector.zeros[Double](sizeOfVector)
      thetas += (topic -> theta)
    })

    // Starts training iterations
    val itemIds = targets.keySet.toList
    for (iteration <- 1 to numOfIterations) {
      // Calculate learning rate
      val eta = 1.0 / (iteration * lambda)

      // Get a new training sample
      val itemId = itemIds(random.nextInt(itemIds.size))
      val target = targets(itemId)
      val tokenGroups = corpus(itemId)
      val vector = SparseVector.zeros[Double](sizeOfVector)
      tokenGroups.foreach(group => {
        val token = group._1
        val tf = group._2
        if (tokenIndices.contains(token) && tokenIndices(token) < sizeOfVector) vector(tokenIndices(token)) = tf.toDouble
      })

      // Update the weight vectors
      topics.foreach(topic => {
        var theta = thetas(topic)
        val y = if (target.contains(topic)) 1.0 else -1.0
        if (y * (theta dot vector) < 1.0) {
          theta = theta * (1.0 - eta * lambda)
          theta = theta + (vector * (eta * y))
        } else {
          theta = theta * (1.0 - eta * lambda)
        }
        theta = theta * 1.0.min((1.0 / math.sqrt(lambda)) / norm(theta))
        thetas += (topic -> theta)
      })

      if (iteration % (numOfIterations / 10) == 0) println("Iteration " + iteration + " of " + numOfIterations + " iterations")
    }
  }

  def predict(tokenGroups: Map[String, Int], maxNumOfClasses: Int): Set[String] = {
    // Calculate feature space dimension including the constant term
    val candidateTopics = topicWeights.keySet

    val vector = SparseVector.zeros[Double](sizeOfVector)
    tokenGroups.foreach(group => {
      val token = group._1
      val tf = group._2
      if (tokenIndices.contains(token) && tokenIndices(token) < sizeOfVector) vector(tokenIndices(token)) = tf.toDouble
    })

    val queue = new mutable.PriorityQueue[TopicPair]()
    candidateTopics.foreach(topic => {
      val theta = thetas(topic)
      val score = theta dot vector
      if (score >= 0) {
        if (queue.size < maxNumOfClasses) queue.enqueue(TopicPair(topic, score))
        else {
          val min = queue.dequeue()
          if (min.score < score) queue.enqueue(TopicPair(topic, score))
          else queue.enqueue(min)
        }
      }
    })

    val predictedTopics = new mutable.HashSet[String]()
    while (queue.nonEmpty) predictedTopics += queue.dequeue().topic
    predictedTopics.toSet
  }
}
