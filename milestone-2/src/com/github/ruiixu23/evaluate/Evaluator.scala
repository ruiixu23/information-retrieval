package com.github.ruiixu23.evaluate

import java.io.{File, FileNotFoundException, IOException}

import com.github.ruiixu23.corpus.{CorpusReader, CorpusUtils}

import scala.collection.mutable
import scala.io.Source

/**
 * Created by ruiixu23 on 23/11/14.
 *
 * Implement the evaluation functions
 */
abstract class Evaluator(protected val isTestingSet: Boolean) {
  protected val corpusFileName = "corpus-converted"
  protected val encoding = "UTF-8"

  protected val targets = new mutable.HashMap[String, Set[String]]()
  protected val corpus = new mutable.HashMap[String, Map[String, Int]]()

  def load(path: String, converted: Boolean): Unit = {
    var numOfDocs = 0
    if (converted) {
      val directory = new File(path)
      if (!directory.exists()) throw new FileNotFoundException("The directory cannot be found at the path:" + path)
      if (!directory.isDirectory) throw new IOException("The resource located at path:" + path + " is not a directory")

      val lines = Source.fromFile(new File(directory, corpusFileName), encoding).getLines()
      while(lines.hasNext) {
        val itemId = lines.next()
        val topicsLine = lines.next()

        val topics = topicsLine.split(",").toSet
        val numOfTokens = lines.next().toInt
        val tokenGroups = new mutable.HashMap[String, Int]()
        for (i <- 0 until numOfTokens) {
          val group = lines.next().split(",", 2)
          val tf = group(0).toInt
          val token = group(1)
          tokenGroups += (token -> tf)
        }
        if (topicsLine.length > 0 || isTestingSet) {
          targets += (itemId -> topics)
          corpus += (itemId -> tokenGroups.toMap)

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
        if (topics.size > 0 || isTestingSet) {
          val tokens = CorpusUtils.removeStopWords(CorpusUtils.tokenizeWithLematizer(doc.getTitle + ". " + doc.getText))

          targets += (itemId -> topics)
          corpus += (itemId -> tokens.groupBy(identity).map(group => group._1 -> group._2.size))

          numOfDocs += 1
          if (numOfDocs % 5000 == 0) println(numOfDocs + " documents loaded")
        }
      }
    }
  }

  def calculateScore(target: Set[String], prediction: Set[String]): (Double, Double, Double) = {
    val precision = if (prediction.size != 0) target.intersect(prediction).size.toDouble / prediction.size else 0.0
    val recall = if (target.size != 0) target.intersect(prediction).size.toDouble / target.size else 0.0
    val f1Score = if (precision + recall != 0) (2 * precision * recall) / (precision + recall) else 0.0
    (precision, recall, f1Score)
  }

  protected def avg(samples: List[Double]): Double = samples.sum / samples.size

  protected def std(samples: List[Double]):Double = {
    val mean = avg(samples)
    var sum = 0.0
    samples.foreach(sample => sum += ((sample - mean) * (sample - mean)))
    sum = sum / samples.size
    math.sqrt(sum)
  }
}