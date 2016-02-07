package com.feynmanliang.kws

import scala.collection.mutable.{HashMap, Set, MultiMap}
import scala.io.BufferedSource
import scala.xml.Elem

case class CTMEntry(
    kwFile: String,
    channel: Int,
    startTime: Double,
    duration: Double,
    token: String,
    score: Double) {
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

class KWSIndex(index: MultiMap[String, CTMEntry]) {
  def get(token: String) = index.get(token)
  def toXML(): Elem = {
    <kwslist
        kwlist_filename="IARPA-babel202b-v1.0d_conv-dev.kwlist.xml"
        language="swahili"
        system_id="">
      {for (kw <- index.keys) yield {
        <detected_kwlist kwid={s"$kw"} oov_count="0" search_time="0.0">
          {for (entry <- index(kw)) yield {
            entry.toXML()
          }}
        </detected_kwlist>
      }}
    </kwslist>
  }
}

object KWSIndex {
  def fromFile(ctmFile: BufferedSource): KWSIndex = {
    val index = ctmFile.getLines().take(20).map { line =>
      val items = line.split(" ")
      val entry = CTMEntry(
        items(0),
        items(1).toInt,
        items(2).toDouble,
        items(3).toDouble,
        items(4),
        items(5).toDouble)
      entry.token -> entry
    }.foldLeft(new HashMap[String, Set[CTMEntry]] with MultiMap[String, CTMEntry]) { (acc, pair) =>
      acc.addBinding(pair._1, pair._2)
    }
    new KWSIndex(index)
  }
}

// vim: set ts=2 sw=2 et sts=2:
