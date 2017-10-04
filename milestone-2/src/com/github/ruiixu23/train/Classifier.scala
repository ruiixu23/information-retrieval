package com.github.ruiixu23.train

import java.io.{File, FileNotFoundException, IOException}

import scala.collection.mutable
import scala.io.Source

/**
 * Created by ruiixu23 on 20/11/14.
 *
 * Abstract class for different classifiers
 */
abstract class Classifier {
  protected val corpusFileName = "corpus-converted"
  protected val encoding = "UTF-8"

  protected case class TopicPair(topic: String, score: Double) extends Ordered[TopicPair] {
    def compare(that: TopicPair) = that.score compare this.score
  }

  def load(path: String, converted: Boolean): Unit
}