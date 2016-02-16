package com.feynmanliang.kws

import org.scalatest.FlatSpec

class QueryResultSpec extends FlatSpec {
  val ctmPath = "lib/ctms/reference.ctm"
  val queryFilePath = "lib/kws/queries.xml"
  val index = KWSIndex(ctmPath)
  val queryResults = index.kws(queryFilePath)

  // TODO: "NONE" vs "STO" score normalization tests
}

// vim: set ts=2 sw=2 et sts=2:
