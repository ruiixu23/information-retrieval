package com.github.ruiixu23.main

import com.github.ruiixu23.corpus.CorpusUtils
import com.github.ruiixu23.evaluate.{LREvaluator, NBEvaluator, SVMEvaluator}
import com.github.ruiixu23.train.{LRClassifier, NBClassifier, SVMClassifier}

/**
 * Created by ruiixu23 on 20/11/14.
 *
 * Starting point of the program
 */sca
object Runner {
  def main(args: Array[String]) {
    if (args.length != 5) {
      throw new IllegalArgumentException("Please provide 5 arguments\n" +
        "Argument 1: the model to use\n" +
        "Argument 2: the path to the training data directory\n" +
        "Argument 3: the path to the validation data directory\n" +
        "Argument 4: the path to the testing data directory\n" +
        "Argument 5: the path to the submission directory\n")
    }

    val model = args(0)
    val trainingDataPath = args(1)
    val validationDataPath = args(2)
    val testingDataPath = args(3)
    val submitDataPath = args(4)

    val conversion = false
    if (conversion) {
      CorpusUtils.convert(trainingDataPath, false)
      CorpusUtils.convert(validationDataPath, false)
      CorpusUtils.convert(testingDataPath, true)
    }
    val converted = false

    model match {
      case "nb" =>
        println("Loading training data")
        val classifier = new NBClassifier()
        classifier.load(trainingDataPath, converted)

        println("Loading validation data")
        val validationEvaluator = new NBEvaluator(false)
        validationEvaluator.load(validationDataPath, converted)

        print("Enter zero to exit, otherwise continue with the number of topics to emit for evaluation: ")
        var flag = Console.readInt()
        var numOfTopics = 0
        while (flag != 0) {
          numOfTopics = flag

          println("Evaluating on validation data")
          validationEvaluator.evaluate(classifier, numOfTopics)

          print("Enter zero to exit, otherwise continue with the number of topics to emit for evaluation: ")
          flag = Console.readInt()
        }

        print("Please choose whether or not to use the last parameters to generate submission files (1: Yes, 0: No): ")
        if (Console.readInt() == 1) {
          println("Generating submission file for the validation set")
          validationEvaluator.submit(classifier, numOfTopics, submitDataPath)

          println("Loading testing data")
          val testingEvaluator = new NBEvaluator(true)
          testingEvaluator.load(testingDataPath, converted)

          println("Generating submission file for the testing set")
          testingEvaluator.submit(classifier, numOfTopics, submitDataPath)
        }
      case "lr" =>
        println("Loading training data")
        val classifier = new LRClassifier()
        classifier.load(trainingDataPath, converted)

        println("Loading validation data")
        val validationEvaluator = new LREvaluator(false)
        validationEvaluator.load(validationDataPath, converted)

        print("Enter zero to exit, otherwise continue with the number of features to use for training: ")
        var flag = Console.readInt()
        var numOfFeatures = 0
        var numOfIterations = 0
        var numOfTopics = 0
        while (flag != 0) {
          numOfFeatures = flag

          print("Choose how many iterations to run for training: ")
          numOfIterations = Console.readInt()

          println("Training the classifier")
          classifier.train(numOfFeatures, numOfIterations)

          print("Enter zero to exit, otherwise continue with the number of topics to emit for evaluation: ")
          flag = Console.readInt()
          while (flag != 0) {
            numOfTopics = flag

            println("Evaluating on validation data")
            validationEvaluator.evaluate(classifier, numOfTopics)

            print("Enter zero to exit, otherwise continue with the number of topics to emit for evaluation: ")
            flag = Console.readInt()
          }

          print("Enter zero to exit, otherwise continue with the number of features to use for training: ")
          flag = Console.readInt()
        }

        print("Please choose whether or not to use the last parameters to generate submission files (1: Yes, 0: No): ")
        if (Console.readInt() == 1) {
          println("Generating submission file for the validation set")
          validationEvaluator.submit(classifier, numOfTopics, submitDataPath)

          println("Loading testing data")
          val testingEvaluator = new LREvaluator(true)
          testingEvaluator.load(testingDataPath, converted)

          println("Generating submission file for the testing set")
          testingEvaluator.submit(classifier, numOfTopics, submitDataPath)
        }

      case "svm" =>
        println("Loading training data")
        val classifier = new SVMClassifier()
        classifier.load(trainingDataPath, converted)

        println("Loading validation data")
        val validationEvaluator = new SVMEvaluator(false)
        validationEvaluator.load(validationDataPath, converted)

        print("Enter zero to exit, otherwise continue with the lambda for training: ")
        var flag = Console.readDouble()
        var lambda = 0.0
        var numOfFeatures = 0
        var numOfIterations = 0
        var numOfTopics = 0
        while (flag != 0) {
          lambda = flag

          print("Choose how many features to use for training: ")
          numOfFeatures = Console.readInt()

          print("Choose how many iterations to run for training: ")
          numOfIterations = Console.readInt()

          println("Training the classifier")
          classifier.train(lambda, numOfFeatures, numOfIterations)

          print("Enter zero to exit, otherwise continue with the number of topics to emit for evaluation: ")
          var intFlag = Console.readInt()
          while (intFlag != 0) {
            numOfTopics = intFlag
            println("Evaluating on validation data")
            validationEvaluator.evaluate(classifier, numOfTopics)

            print("Enter zero to exit, otherwise continue with the number of topics to emit for evaluation: ")
            intFlag = Console.readInt()
          }

          print("Enter zero to exit, otherwise continue with the lambda for training: ")
          flag = Console.readDouble()
        }

        print("Please choose whether or not to use the last parameters to generate submission files (1: Yes, 0: No): ")
        if (Console.readInt() == 1) {
          println("Generating submission file for the validation set")
          validationEvaluator.submit(classifier, numOfTopics, submitDataPath)

          println("Loading testing data")
          val testingEvaluator = new SVMEvaluator(true)
          testingEvaluator.load(testingDataPath, converted)

          println("Generating submission file for the testing set")
          testingEvaluator.submit(classifier, numOfTopics, submitDataPath)
        }
    }
  }
} 