package com.feynmanliang.kws

import org.scalatest.FlatSpec

class QueryResultSpec extends FlatSpec {
  val ctmPath = "lib/ctms/reference.ctm"
  val queryFilePath = "lib/kws/queries.xml"
  val index = KWSIndex(ctmPath)
  val queryResults = index.kws(queryFilePath)

  // TODO: "NONE" vs "STO" score normalization tests
  "Multiple QueryResults" should "be able to be read from XML" in {
    val qr = QueryResult.fromXML(scala.xml.XML.loadFile("lib/kws/morph.xml"))
    assert(qr.results.get("KW202-00001").isDefined)
  }
}

// vim: set ts=2 sw=2 et sts=2:
