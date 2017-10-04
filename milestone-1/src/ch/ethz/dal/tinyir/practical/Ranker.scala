package ch.ethz.dal.tinyir.practical

import scala.collection.immutable.TreeMap
import scala.collection.mutable.HashMap
import scala.collection.mutable.PriorityQueue
import scala.io.Source
import scala.math.log

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.util.StopWatch

case class DocumentPair(docId: String, score: Double) extends Ordered[DocumentPair] {
  def compare(that: DocumentPair) = this.score compare that.score
}

class Ranker {
  val stopWords = Set(
    "",
    "I",
    "a",
    "about",
    "an",
    "are",
    "as",
    "at",
    "be",
    "by",
    "com",
    "for",
    "from",
    "how",
    "in",
    "is",
    "it",
    "of",
    "on",
    "or",
    "that",
    "the",
    "this",
    "to",
    "was",
    "what",
    "when",
    "where",
    "who",
    "will",
    "with",
    "the",
    "www")

  val moreStopWords = Set(
    "",
    "a",
    "about",
    "above",
    "after",
    "again",
    "against",
    "all",
    "am",
    "an",
    "and",
    "any",
    "are",
    "aren't",
    "as",
    "at",
    "be",
    "because",
    "been",
    "before",
    "being",
    "below",
    "between",
    "both",
    "but",
    "by",
    "can't",
    "cannot",
    "could",
    "couldn't",
    "did",
    "didn't",
    "do",
    "does",
    "doesn't",
    "doing",
    "don't",
    "down",
    "during",
    "each",
    "few",
    "for",
    "from",
    "further",
    "had",
    "hadn't",
    "has",
    "hasn't",
    "have",
    "haven't",
    "having",
    "he",
    "he'd",
    "he'll",
    "he's",
    "her",
    "here",
    "here's",
    "hers",
    "herself",
    "him",
    "himself",
    "his",
    "how",
    "how's",
    "i",
    "i'd",
    "i'll",
    "i'm",
    "i've",
    "if",
    "in",
    "into",
    "is",
    "isn't",
    "it",
    "it's",
    "its",
    "itself",
    "let's",
    "me",
    "more",
    "most",
    "mustn't",
    "my",
    "myself",
    "no",
    "nor",
    "not",
    "of",
    "off",
    "on",
    "once",
    "only",
    "or",
    "other",
    "ought",
    "our",
    "ours  ",
    "ourselves",
    "out",
    "over",
    "own",
    "same",
    "shan't",
    "she",
    "she'd",
    "she'll",
    "she's",
    "should",
    "shouldn't",
    "so",
    "some",
    "such",
    "than",
    "that",
    "that's",
    "the",
    "their",
    "theirs",
    "them",
    "themselves",
    "then",
    "there",
    "there's",
    "these",
    "they",
    "they'd",
    "they'll",
    "they're",
    "they've",
    "this",
    "those",
    "through",
    "to",
    "too",
    "under",
    "until",
    "up",
    "very",
    "was",
    "wasn't",
    "we",
    "we'd",
    "we'll",
    "we're",
    "we've",
    "were",
    "weren't",
    "what",
    "what's",
    "when",
    "when's",
    "where",
    "where's",
    "which",
    "while",
    "who",
    "who's",
    "whom",
    "why",
    "why's",
    "with",
    "won't",
    "would",
    "wouldn't",
    "you",
    "you'd",
    "you'll",
    "you're",
    "you've",
    "your",
    "yours",
    "yourself",
    "yourselves")

  var logIdfs = new HashMap[String, Double]
  var logTfs = new HashMap[String, Map[String, Double]]
  var docModel = new HashMap[String, (Int, Map[String, Int])]
  var collectionModel = new HashMap[String, Int]
  var collectionLength: Long = 0

  def tokenize(text: String): List[String] = text.split("[ .,;:?!\t\n\r\f]+").toList

  def removeStopwords(tokens: List[String]) = tokens.filterNot(token => stopWords.contains(token))

  /**
   * Parses the query document and return all the queries with each represented as a string in a map
   * whose key is the topic id for the corresponding query
   */
  def parseQueries(topics: String): Map[String, String] = {
    val docString = Source.fromFile(topics).mkString

    val topicNumberPattern = """(?s)<num>\s*Number:\s*(\d\d\d).+?<dom>""".r
    val topicPattern = """(?s)<title>\s*Topic:\s*(.+?)<desc>""".r

    var queries = new TreeMap[String, String]
    for ((topicNumberPattern(num), topicPattern(topic)) <- (topicNumberPattern.findAllIn(docString) zip topicPattern.findAllIn(docString))) {
      queries = queries + (num.toLowerCase -> topic.toLowerCase)
    }

    queries

  }

  /**
   * Parse the document corpus to obtain both (log) term frequency and (log) inverse document frequency
   * and store in the fields logTfs and logIdfs
   */
  def parseCorpus(corpus: String) = {
    val sw = new StopWatch
    sw.start

    val tipster = new TipsterStream(corpus)
    val numOfDocs = tipster.length

    val dfs = new HashMap[String, Int]

    var iteration = 0
    for (doc <- tipster.stream) {
      iteration += 1

      val docId = doc.name
      val tokens = removeStopwords(tokenize(doc.body.toLowerCase))

      val tf = tokens.groupBy(identity).mapValues(list => log(list.length + 1.0))
      tf.foreach(pair => {
        val token = pair._1
        dfs += (token -> (dfs.getOrElse(token, 0) + 1))
      })

      logTfs += (docId -> tf)

      if (iteration % 10000 == 0) {
        println("Iteration = " + iteration + "/" + numOfDocs)
        System.gc()
      }

    }
  }

  /**
   * Parse the document corpus to obtain only the (log) inverse document frequency and store it
   * in the field logIdfs.
   */
  def parseCorpusInverseDocumentFrequency(corpus: String) = {
    val sw = new StopWatch
    sw.start

    val tipster = new TipsterStream(corpus)
    val numOfDocs = tipster.length
    val dfs = new HashMap[String, Int]

    var iteration = 0
    for (doc <- tipster.stream) {
      iteration += 1

      val docId = doc.name
      val tokens = removeStopwords(tokenize(doc.body.toLowerCase)).distinct
      for (token <- tokens) {
        dfs += (token -> (dfs.getOrElse(token, 0) + 1))
      }

      if (iteration % 10000 == 0) {
        println("Iteration = " + iteration + "/" + numOfDocs)
        System.gc()
      }

    }

    for (token <- dfs.keySet) {
      logIdfs += (token -> (log(numOfDocs / dfs(token).toDouble)))
    }

    sw.stop
    println("Getting document frequency time = " + sw.stopped)
  }

  /**
   * Parse the document corpus to obtain only the (log) term frequency and store it
   * in the field logTfs.
   */
  def parseCorpusTermFrequency(corpus: String) = {
    val sw = new StopWatch
    sw.start

    val tipster = new TipsterStream(corpus)
    val numOfDocs = tipster.length

    var iteration = 0
    for (doc <- tipster.stream) {
      iteration += 1

      val docId = doc.name
      val tokens = removeStopwords(tokenize(doc.body.toLowerCase))
      val tf = tokens.groupBy(identity).mapValues(list => log(list.length + 1.0))
      logTfs += (docId -> tf)

      if (iteration % 10000 == 0) {
        println("Iteration = " + iteration + "/" + numOfDocs)
        System.gc()
      }
    }

    sw.stop
    println("Getting term frequency time = " + sw.stopped)
  }

  /**
   * Parse the document corpus to obtain both document-wise and collection-wise language distribution
   */
  def parseCorpusLanguageModel(corpus: String) = {
    val sw = new StopWatch
    sw.start

    val tipster = new TipsterStream(corpus)
    val numOfDocs = tipster.length

    var iteration = 0
    for (doc <- tipster.stream) {
      iteration += 1

      val docId = doc.name
      val tokens = removeStopwords(tokenize(doc.body.toLowerCase))
      val tf: Map[String, Int] = tokens.groupBy(identity).mapValues(list => list.length)
      docModel += (docId -> (tokens.length, tf))

      for (token <- tf.keySet) {
        collectionModel += (token -> (collectionModel.getOrElse(token, 0) + tf(token)))
      }

      collectionLength += tokens.length

      if (iteration % 10000 == 0) {
        println("Iteration = " + iteration + "/" + numOfDocs)
        System.gc()
      }
    }

    sw.stop
    println("Getting language model time = " + sw.stopped)
  }

  /**
   * Parse the document corpus to obtain only the collection-wise language distribution
   */
  def parseCorpusLanguageModelCollectionStats(corpus: String) = {
    val sw = new StopWatch
    sw.start

    val tipster = new TipsterStream(corpus)
    val numOfDocs = tipster.length

    var iteration = 0
    for (doc <- tipster.stream) {
      iteration += 1

      val tokens = removeStopwords(tokenize(doc.body.toLowerCase))
      val tf: Map[String, Int] = tokens.groupBy(identity).mapValues(list => list.length)

      for (pair <- tf) {
        collectionModel += (pair._1 -> (collectionModel.getOrElse(pair._1, 0) + pair._2))
      }

      collectionLength += tokens.length

      if (iteration % 10000 == 0) {
        println("Iteration = " + iteration + "/" + numOfDocs)
        System.gc()
      }
    }

    sw.stop
    println("Getting language model time = " + sw.stopped)
  }

  /**
   * Rank the document using the tf-idf model. This method ranks one query at a time. It does not
   * need to go through the document corpus again.
   */
  def rank(query: String, numberOfRetrieval: Int): List[String] = {
    var queue = new PriorityQueue[DocumentPair]()
    for (docId <- logTfs.keySet) {
      val terms = removeStopwords(tokenize(query.toLowerCase).distinct)
      var score = 0.0

      for (term <- terms) {
        score += logTfs(docId).getOrElse(term, 0.0) * logIdfs.getOrElse(term, 0.0)
      }

      queue.enqueue(DocumentPair(docId, score))
      if (queue.size > numberOfRetrieval) {
        queue.dequeue()
      }
    }

    queue.map(_.docId).toList
  }

  /**
   * Rank the document using the tf-idf model. This method ranks all queries simultaneously.
   * It needs to go through the document corpus once.
   */
  def rank(corpus: String, queries: Map[String, String], numberOfRetrieval: Int): HashMap[String, List[String]] = {
    var queues = new HashMap[String, PriorityQueue[DocumentPair]]()
    for (topic <- queries.keySet) {
      queues += (topic -> new PriorityQueue[DocumentPair]())
    }

    val tipster = new TipsterStream(corpus)
    val numOfDocs = tipster.length

    var iteration = 0
    for (doc <- tipster.stream) {
      iteration += 1

      val docId = doc.name
      val tokens = removeStopwords(tokenize(doc.body.toLowerCase))
      val tf = tokens.groupBy(identity).mapValues(list => log(list.length + 1.0))

      for (topic <- queries.keySet) {
        val terms = removeStopwords(tokenize(queries(topic).toLowerCase).distinct)
        var score = 0.0
        for (term <- terms) {
          score += tf.getOrElse(term, 0.0) * logIdfs.getOrElse(term, 0.0)
        }

        queues(topic).enqueue(DocumentPair(docId, score))
        if (iteration > numberOfRetrieval) {
          queues(topic).dequeue()
        }
      }

      if (iteration % 10000 == 0) {
        println("Iteration = " + iteration + "/" + numOfDocs)
        System.gc()
      }
    }

    var ranks = new HashMap[String, List[String]]()
    for (topic <- queues.keySet) {
      ranks += (topic -> (queues(topic).map(_.docId).toList))
    }

    ranks
  }

  /**
   * Rank the document using the linear interpolation language model. This method receives one
   * query at a time and returns a list of document IDs that are relevant to he query. It does
   * not need to go through the document corpus again.
   */
  def rank(query: String, lambda: Double, numberOfRetrieval: Int): List[String] = {
    var queue = new PriorityQueue[DocumentPair]()
    for (docId <- docModel.keySet) {
      val terms = removeStopwords(tokenize(query.toLowerCase).distinct)
      var score = 0.0

      val docLength = docModel(docId)._1
      val tf = docModel(docId)._2
      for (term <- terms) {
        val docP = tf.getOrElse(term, 0) / docLength.toDouble
        val colP = collectionModel.getOrElse(term, 0) / collectionLength.toDouble
        if (docP + colP > 0)
          score += log(lambda * docP + (1 - lambda) * colP)
      }

      queue.enqueue(DocumentPair(docId, score))
      if (queue.size > numberOfRetrieval) {
        queue.dequeue()
      }

    }

    queue.map(_.docId).toList
  }

  /**
   * Rank the document using the linear interpolation language model. This method simultaneously
   * ranks all the queries. It needs to go through the document corpus once more.
   */
  def rank(corpus: String, queries: Map[String, String], lambda: Double, numberOfRetrieval: Int): HashMap[String, List[String]] = {
    var queues = new HashMap[String, PriorityQueue[DocumentPair]]()
    for (topic <- queries.keySet) {
      queues += (topic -> new PriorityQueue[DocumentPair]())
    }

    val tipster = new TipsterStream(corpus)
    val numOfDocs = tipster.length

    var iteration = 0
    for (doc <- tipster.stream) {
      iteration += 1

      val docId = doc.name
      val tokens = removeStopwords(tokenize(doc.body.toLowerCase))
      val docLength = tokens.length
      val tf: Map[String, Int] = tokens.groupBy(identity).mapValues(list => list.length)

      for (topic <- queries.keySet) {
        val terms = removeStopwords(tokenize(queries(topic).toLowerCase).distinct)
        var score = 0.0

        for (term <- terms) {
          val docP = tf.getOrElse(term, 0) / docLength.toDouble
          val colP = collectionModel.getOrElse(term, 0) / collectionLength.toDouble
          if (docP + colP > 0)
            score += log(lambda * docP + (1 - lambda) * colP)
        }

        queues(topic).enqueue(DocumentPair(docId, score))
        if (iteration > numberOfRetrieval) {
          queues(topic).dequeue()
        }
      }

      if (iteration % 10000 == 0) {
        println("Iteration = " + iteration + "/" + numOfDocs)
        System.gc()
      }
    }

    var ranks = new HashMap[String, List[String]]()
    for (topic <- queues.keySet) {
      ranks += (topic -> (queues(topic).map(_.docId).toList))
    }

    ranks
  }
}
