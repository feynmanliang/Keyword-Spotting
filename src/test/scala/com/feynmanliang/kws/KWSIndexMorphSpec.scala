package com.feynmanliang.kws

import org.scalatest.FlatSpec

class KWSIndexMorphSpec extends FlatSpec {
  val ctmPath = "lib/ctms/reference.ctm"
  val queryFilePath = "lib/kws/queries.xml"
  val obPath = "lib/dicts/morph.dct"
  val qPath = "lib/dicts/morph.kwslist.dct"

  val indexMorph = KWSIndexMorph(ctmPath, obPath, qPath)

  "A KWSIndexMorph" should "be queryable" in {
    val queryResults = indexMorph.kws(queryFilePath)
    assert((queryResults.toXML() \ "detected_kwlist").length > 100)
  }
}


// vim: set ts=2 sw=2 et sts=2:
