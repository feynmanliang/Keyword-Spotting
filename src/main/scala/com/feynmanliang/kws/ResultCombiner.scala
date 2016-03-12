package com.feynmanliang.kws

import java.io.File
import scala.xml.Elem

object ResultCombiner {
  def combine(qr1: QueryResult, qr2: QueryResult): QueryResult = qr1.copy(
    scoreNormalization="NONE", // do not renormalize resultsNorm since it is reused in accumulator
    results=(qr1.results.keys++qr2.results.keys).map { query =>
      val qr1Hits = qr1.resultsNorm.getOrElse(query, Set())
      val qr2Hits = qr2.resultsNorm.getOrElse(query, Set())

      val combinedHits = (qr1Hits ++ qr2Hits)
        .foldLeft(List.empty[CTMEntry]) { case (combinedHits, hit) =>
          // acc will be a list of non-overlapping hits combined from qr1 and qr2
          combinedHits
            .find(oHit => oHit.overlapsWith(hit))
            match {
              case None => hit :: combinedHits
              case Some(oHit) => {
                // TODO: comment on how order of combination matters
                val oHitNew = (if (hit.score > oHit.score) hit else oHit).copy(
                  score = hit.score + oHit.score // SUM combination, TODO: investigate others
                )
                combinedHits.updated(combinedHits.indexOf(oHit), oHitNew)
              }
            }
        }
        query -> combinedHits.toSet
    }.toMap)

  def main(args: Array[String]):Unit = {
    case class Config(
      postingLists: Seq[File] = Seq(),
      scoreNorms: Seq[String] = Seq(),
      finalScoreNorm: String = "NONE",
      out: File = new File("."))

    val parser = new scopt.OptionParser[Config]("ResultCombiner") {
      head("resultCombiner")
      opt[Seq[File]]('p', "postingLists") required() valueName("<postingList1>,...") action { (x,c) =>
      c.copy(postingLists = x) } text("posting lists to combine")
      opt[Seq[String]]('n', "scoreNorms") required() valueName("<scoreNorm1>,...") action { (x,c) =>
      c.copy(scoreNorms = x) } text("score normalizations to apply before combining (same order as posting lists)")
      opt[String]('n', "finalScoreNorm") required() valueName("<finalScoreNorm>") action { (x,c) =>
      c.copy(finalScoreNorm = x) } text("score normalizations to apply to combined output")
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
        scala.xml.XML.save(
          config.out.getPath(),
          combinedQr.copy(scoreNormalization = config.finalScoreNorm).toXML())
      case None =>
        // arguments are bad, error message will have been displayed
        sys.error("Error parsing arguments!")
    }
  }
}

// vim: set ts=2 sw=2 et sts=2:
