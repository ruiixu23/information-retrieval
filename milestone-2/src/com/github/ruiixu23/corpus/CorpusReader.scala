package com.github.ruiixu23.corpus

import java.io.{File, FileNotFoundException, IOException}
import java.util.zip.ZipFile

import scala.collection.mutable

/**
 * Created by ruiixu23 on 20/11/14.
 */
class CorpusReader(path: String) extends Iterator[ReuterDocument] {
  private val zips: mutable.Queue[File] = {
    val zipDirectory = new File(path)
    if (!zipDirectory.exists()) throw new FileNotFoundException("The directory containing the corpus cannot be found at the path:" + path)
    if (!zipDirectory.isDirectory) throw new IOException("The resource located at path:" + path + " is not a directory")

    mutable.Queue[File](zipDirectory.listFiles().filter(file => file.getName.endsWith(".zip")): _*)
  }

  private var zipFile = {
    if (zips.nonEmpty) {
      new ZipFile(zips.dequeue())
    } else {
      null
    }
  }

  private var zipFileEntries = {
    if (zipFile != null) {
      zipFile.entries()
    } else {
      null
    }
  }

  override def hasNext: Boolean = {
    if (zipFileEntries != null && zipFileEntries.hasMoreElements)
      true
    else {
      while (zipFileEntries != null && !zipFileEntries.hasMoreElements && zips.nonEmpty) {
        if (zipFile != null) zipFile.close()

        zipFile = new ZipFile(zips.dequeue())
        zipFileEntries = zipFile.entries()
      }
      zipFileEntries.hasMoreElements
    }
  }

  override def next(): ReuterDocument = {
    if (zipFileEntries != null && zipFileEntries.hasMoreElements) {
      val entry = zipFileEntries.nextElement()
      val is = zipFile.getInputStream(entry)
      val document = ReuterDocument.parse(is)
      is.close()
      document
    } else {
      null
    }
  }
}