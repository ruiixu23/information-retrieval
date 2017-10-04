package com.github.ruiixu23.evaluate

import java.io.{File, FileNotFoundException, IOException, PrintWriter}

import com.github.ruiixu23.train.SVMClassifier

import scala.collection.mutable

/**
 * Created by ruiixu23 on 01/12/14.
 *
 * Implement the evaluation functions for the SVM classifier
 */
class SVMEvaluator(override val isTestingSet: Boolean) extends Evaluator(isTestingSet) {
  protected val validationSubmitFileName = "classify-ruifeng-xu-l-svm.run"
  protected val testSubmissionFileName = "classify-ruifeng-xu-u-svm.run"

  def evaluate(classifier: SVMClassifier,  maxNumOfTopics: Int): Unit = {
    var numOfDocs = 0

    val precision = new mutable.ListBuffer[Double]()
    val recall = new mutable.ListBuffer[Double]()
    val f1Score = new mutable.ListBuffer[Double]()

    corpus.foreach(i => {
      val itemId = i._1
      val prediction = classifier.predict(i._2, maxNumOfTopics)
      val result = calculateScore(targets(itemId), prediction)
      precision.append(result._1)
      recall.append(result._2)
      f1Score.append(result._3)
      numOfDocs += 1
      if (numOfDocs % 5000 == 0) println(numOfDocs + " predictions finished")
    })

    val precisionMean = avg(precision.toList)
    val recallMean = avg(recall.toList)
    val f1ScoreMean = avg(f1Score.toList)

    val precisionStd = std(precision.toList)
    val recallStd = std(recall.toList)
    val f1ScoreStd = std(f1Score.toList)

    println("Maximum number of topics: " + maxNumOfTopics)
    println("Precision: Mean=" + precisionMean + " Std=" + precisionStd)
    println("Recall: Mean=" + recallMean + " Std=" + recallStd)
    println("F1 Score: Mean=" + f1ScoreMean + " Std=" + f1ScoreStd)
  }

  def submit(classifier: SVMClassifier, maxNumOfTopics: Int, path: String): Unit = {
    val directory = new File(path)
    if (!directory.exists()) throw new FileNotFoundException("The directory cannot be found at the path:" + path)
    if (!directory.isDirectory) throw new IOException("The resource located at path:" + path + " is not a directory")

    val predictions = new mutable.HashMap[String, Set[String]]()
    var numOfDocs = 0
    corpus.foreach(i => {
      predictions += (i._1 -> classifier.predict(i._2, maxNumOfTopics))
      numOfDocs += 1
      if (numOfDocs % 5000 == 0) println(numOfDocs + " predictions finished")
    })

    val submitFileName = if (isTestingSet) testSubmissionFileName else validationSubmitFileName
    val submitOut = new PrintWriter(new File(directory, submitFileName), encoding)
    try {
      if (!isTestingSet) {
        val precision = new mutable.ListBuffer[Double]()
        val recall = new mutable.ListBuffer[Double]()
        val f1Score = new mutable.ListBuffer[Double]()

        targets.keysIterator.foreach(itemId => {
          val result = calculateScore(targets(itemId), predictions(itemId))
          precision.append(result._1)
          recall.append(result._2)
          f1Score.append(result._3)
        })

        val precisionMean = avg(precision.toList)
        val recallMean = avg(recall.toList)
        val f1ScoreMean = avg(f1Score.toList)

        submitOut.println(precisionMean + " " + recallMean + " " + f1ScoreMean)
      }

      predictions.foreach(i => submitOut.println(i._1 + " " + i._2.mkString(" ")))
    }
    finally submitOut.close()
  }
}
