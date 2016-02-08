package com.feynmanliang.kws

import org.scalatest.FlatSpec

class KWSIndexMorphSpec extends FlatSpec {
  val ctmPath = "lib/ctms/reference.ctm"
  val queryFilePath = "lib/kws/queries.xml"
  val obPath = "lib/dicts/morph.dct"
  val qPath = "lib/dicts/morph.kwslist.dct"

  val indexMorph = KWSIndexMorph(ctmPath, queryFilePath, obPath, qPath)

  "A KWSIndexMorph" should "be queryable" in {
    val queryResults = indexMorph.kws(queryFilePath)
    println(queryResults.toXML())
  }
}


// vim: set ts=2 sw=2 et sts=2:
