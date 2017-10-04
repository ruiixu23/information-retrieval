package ch.ethz.dal.tinyir.practical

import java.io.PrintWriter

class RankerTipster extends Ranker {

}

object RankerTipster {

  /**
   * args[0]: The absolute path to the folder containing all corpus
   * args[1]: The absolute path to the file topics
   * args[2]: The ranking model to use with "T" for term based model and "L" for language model
   * args[3]: The number indicating how many top-ranking documents to return
   * args[4]: The absolute path to the file qrels
   */

  def main(args: Array[String]) {
    //        val corpus = "./corpus"
    //        val topics = "./topics-to-submit"
    //        val model = "L"
    //        val numberOfRetrieval = 100
    //        val qrels = "./qrels"

    val corpus = args(0)
    val topics = args(1)
    val model = args(2)
    val numberOfRetrieval = args(3).toInt
    val qrels = args(4)

    val ranker = new RankerTipster
    val queries = ranker.parseQueries(topics)

    model match {
      case "T" => {
        ranker.parseCorpusInverseDocumentFrequency(corpus)
        //        ranker.parseCorpusTermFrequency(corpus)

        val ranks = ranker.rank(corpus, queries, numberOfRetrieval)

        val writer = new PrintWriter("ranking-t-firstname-lastname.run")
        //        for (key <- queries.keys) {
        //          val rank = ranker.rank(queries(key), numberOfRetrieval)
        //          rank.zipWithIndex.foreach(x => writer.println(key + " " + (x._2 + 1) + " " + x._1))
        //        }
        for (topic <- ranks.keySet.toList.sorted) {
          val rank = ranks(topic)
          rank.zipWithIndex.foreach(x => writer.println(topic.toInt + " " + (x._2 + 1) + " " + x._1))
        }

        writer.close()
      }
      case "L" => {
        println("Please input a value (0, 1) for lambda")
        val lambda = readDouble
        //        ranker.parseCorpusLanguageModel(corpus)
        ranker.parseCorpusLanguageModelCollectionStats(corpus)
        val ranks = ranker.rank(corpus, queries, lambda, numberOfRetrieval)
        val writer = new PrintWriter("ranking-l-firstname-lastname.run")
        //        for (key <- queries.keys) {
        //          val rank = ranker.rank(queries(key), lambda, numberOfRetrieval)
        //          rank.zipWithIndex.foreach(x => writer.println(key + " " + (x._2 + 1) + " " + x._1))
        //        }
        for (topic <- ranks.keySet.toList.sorted) {
          val rank = ranks(topic)
          rank.zipWithIndex.foreach(x => writer.println(topic.toInt + " " + (x._2 + 1) + " " + x._1))
        }
        writer.close()
      }
    }
  }
}