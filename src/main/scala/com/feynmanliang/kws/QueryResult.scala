package com.feynmanliang.kws

import scala.xml.Elem

case class QueryResult(
    val file: String,
    private[kws] val results: Map[String, Set[CTMEntry]],
    scoreNormalization: String = "NONE") {

  val resultsNorm = scoreNormalization match {
    case "NONE" => results
    case "STO" => results.map { case (k,v) =>
      val sumScore = v.map(_.score).sum
      k -> v.map(entry => entry.copy(
        score = entry.score / sumScore
      ))
    }
    case _ => sys.error(s"Unknown score normalization, got: ${scoreNormalization}")
  }

  def toXML(): Elem = {
    <kwslist
        kwlist_filename="IARPA-babel202b-v1.0d_conv-dev.kwlist.xml"
        language="swahili"
        system_id="">
      {for (kw <- resultsNorm.keys) yield {
        <detected_kwlist kwid={s"$kw"} oov_count="0" search_time="0.0">
          {for {
            entry <- resultsNorm(kw)
            // if (entry.score >= 0.042)
          } yield {
            entry.toXML()
          }}
        </detected_kwlist>
      }}
    </kwslist>
  }
}

// vim: set ts=2 sw=2 et sts=2:
