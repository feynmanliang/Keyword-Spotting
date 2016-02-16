package com.feynmanliang.kws

import scala.xml.Elem

case class CTMEntry(
    kwFile: String,
    channel: Int,
    startTime: Double,
    duration: Double,
    token: String,
    prevToken: Option[String],
    prevEndTime: Double, // for testing contiguity during phrase query
    score: Double) extends Ordered[CTMEntry] {
  import scala.math.Ordered.orderingToOrdered

  def compare(that: CTMEntry): Int =
    (this.kwFile, this.startTime) compare (that.kwFile, that.startTime)

  def toXML(): Elem = {
    <kw
      file={kwFile}
      channel={f"$channel"}
      tbeg={f"$startTime%.2f"}
      dur={f"$duration%.2f"}
      score={f"$score%.6f"}
      decision="YES" />
  }
}

// vim: set ts=2 sw=2 et sts=2:
