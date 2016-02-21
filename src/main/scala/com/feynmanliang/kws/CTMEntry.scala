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

object CTMEntry {
  def fromXMLNode(e: scala.xml.Node): CTMEntry = CTMEntry(
    kwFile=(e \ "@file").text,
    channel=(e \ "@channel").text.toInt,
    startTime=(e \ "@tbeg").text.toDouble,
    duration=(e \ "@dur").text.toDouble,
    token="", // TODO: allow initializing token from XML (OK for just combining)
    prevToken=None, // TODO: allow initializing token from XML (OK for just combining)
    prevEndTime=0D, // TODO: allow initializing token from XML (OK for just combining)
    score=(e \ "@score").text.toDouble)
}

// vim: set ts=2 sw=2 et sts=2:
