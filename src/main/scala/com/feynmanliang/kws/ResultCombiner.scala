package com.feynmanliang.kws

import java.io.File
import scala.xml.Elem

object ResultCombiner {
  def combine(qr1: QueryResult, qr2: QueryResult): QueryResult = qr1.copy(
    results=(qr1.results.keys++qr2.results.keys).map {
      k => k -> (qr1.results.getOrElse(k,Set()) ++ qr2.results.getOrElse(k,Set()))
    }.toMap)

  def main(args: Array[String]):Unit = {
    case class Config(
      postingLists: Seq[File] = Seq(),
      scoreNorms: Seq[String] = Seq(),
      out: File = new File("."))

    val parser = new scopt.OptionParser[Config]("ResultCombiner") {
      head("resultCombiner")
      opt[Seq[File]]('p', "postingLists") required() valueName("<postingList1>,...") action { (x,c) =>
      c.copy(postingLists = x) } text("posting lists to combine")
      opt[Seq[String]]('n', "scoreNorms") required() valueName("<scoreNorm1>,...") action { (x,c) =>
      c.copy(scoreNorms = x) } text("score normalizations to apply (same order as posting lists)")
      opt[File]('o', "out") required() valueName("<file>") action { (x, c) =>
      c.copy(out = x) } text("file to output to")
    }
    parser.parse(args, Config()) match {
      case Some(config) =>
        require(config.postingLists.size == config.scoreNorms.size)
        val combinedQr = config.postingLists.zip(config.scoreNorms)
          .map { case (pl, sn) =>
            QueryResult.fromXML(scala.xml.XML.load(pl.getPath()), sn)
          }
          .reduceLeft(ResultCombiner.combine)
        scala.xml.XML.save(config.out.getPath(), combinedQr.toXML())
      case None =>
        // arguments are bad, error message will have been displayed
        sys.error("Error parsing arguments!")
    }
  }
}

// vim: set ts=2 sw=2 et sts=2:
