package com.github.ruiixu23.corpus

import java.io.InputStream
import javax.xml.parsers.DocumentBuilderFactory

import org.w3c.dom.{Document, Node}

/**
 * Created by ruiixu23 on 20/11/14.
 *
 * Implement the Reuter document
 */
class ReuterDocument(private val itemId: String, private val date: String, private val title: String,
                     private val headline: String, private val text: String, private val topics: Set[String]) {
  /**
   * Get the id of the news item
   */
  def getItemId = itemId

  /**
   * Get the publishing date of the news item
   */
  def getDate = date

  /**
   * Get the title of the news item
   */
  def getTitle = title

  /**
   * Get the headline of the news item
   */
  def getHeadline = headline

  /**
   * Get the text of the news item
   */
  def getText = text

  /**
   * Get the topics of the news item
   */
  def getTopics = topics
}

object ReuterDocument {
  /**
   * Parse the document from the input stream and return a instance of Reuter document object
   */
  def parse(is: InputStream): ReuterDocument = {
    val dbFactory = DocumentBuilderFactory.newInstance
    val dBuilder = dbFactory.newDocumentBuilder
    val xml = dBuilder.parse(is)

    val newsItem = getFirstNodeByTagName(xml, "newsitem")
    if (newsItem != null) {
      val itemId = getAttribute(newsItem, "itemid")
      val date = getAttribute(newsItem, "date")

      val titleNode = getFirstNodeByTagName(xml, "title")
      val title = if (titleNode == null) null else titleNode.getTextContent

      val headlineNode = getFirstNodeByTagName(xml, "headline")
      val headline = if (headlineNode == null) null else headlineNode.getTextContent

      val text = parseText(xml)

      val topics = parseTopics(xml)

      new ReuterDocument(itemId, date, title, headline, text, topics)
    } else {
      null
    }
  }

  /**
   * Get attribute from a node. Returns null if the attribute does not exist.
   */
  private def getAttribute(node: Node, attributeName: String): String = {
    if (node.hasAttributes) {
      val attribute = node.getAttributes.getNamedItem(attributeName)
      if (attribute != null) attribute.getTextContent else null
    } else {
      null
    }
  }

  /**
   * Get the first node from the XML file with the specified tag name
   */
  private def getFirstNodeByTagName(doc: Document, tagName: String): Node = {
    val nodeList = doc.getElementsByTagName(tagName)
    if (nodeList.getLength >= 1) {
      nodeList.item(0)
    } else {
      null
    }
  }

  /**
   * Get the text of the news item
   */
  private def parseText(doc: Document): String = {
    val textNode = getFirstNodeByTagName(doc, "text")
    if (textNode == null) {
      null
    } else {
      val stringBuilder = new StringBuilder()
      val paragraphList = textNode.getChildNodes
      for (i <- 0 until paragraphList.getLength) {
        stringBuilder.append(paragraphList.item(i).getTextContent)
      }
      stringBuilder.toString()
    }
  }

  /**
   * Get the topics of the news item
   */
  private def parseTopics(doc: Document): Set[String] = {
    val topics = new scala.collection.mutable.HashSet[String]

    val codesList = doc.getElementsByTagName("codes")
    for (i <- 0 until codesList.getLength) {
      val codes = codesList.item(i)
      if (codes.hasAttributes) {
        val classValue = getAttribute(codes, "class")
        if (classValue != null && classValue.contains("topics")) {
          val codeList = codes.getChildNodes
          for (j <- 0 until codeList.getLength) {
            val code = codeList.item(j)
            val topic = getAttribute(code, "code")
            if (topic != null) {
              topics += topic
            }
          }
        }
      }
    }

    topics.toSet
  }
}