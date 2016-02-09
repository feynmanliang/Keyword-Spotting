package com.feynmanliang.kws

import scala.xml.Elem

class QueryResult(
    val file: String,
    private[kws] val results: Map[String, Set[CTMEntry]]) {
  def toXML(): Elem = {
    <kwslist
        kwlist_filename="IARPA-babel202b-v1.0d_conv-dev.kwlist.xml"
        language="swahili"
        system_id="">
      {for (kw <- results.keys) yield {
        <detected_kwlist kwid={s"$kw"} oov_count="0" search_time="0.0">
          {for (entry <- results(kw)) yield {
            entry.toXML()
          }}
        </detected_kwlist>
      }}
    </kwslist>
  }
}

// vim: set ts=2 sw=2 et sts=2:
