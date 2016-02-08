package com.feynmanliang.kws

import org.scalatest.FlatSpec

class KWSIndexSpec extends FlatSpec {

  "A KWSIndex" should "build a KWSIndex with fromFile" in {
    val ctmPath = "lib/ctms/reference.ctm"
    val index = KWSIndex.fromFile(ctmPath)
    assert(!index.get("halo").isEmpty)
  }

  it should "perform KWS when given a queryFile" in {
    val ctmPath = "lib/ctms/reference.ctm"
    val index = KWSIndex.fromFile(ctmPath)

    val queryFilePath = "lib/kws/queries.xml"
    val queryResults = index.kws(queryFilePath)

    assert(queryResults.file === "queries.xml")
    assert(!(queryResults.toXML() \ "detected_kwlist").isEmpty)
  }

  //it should "produce NoSuchElementException when head is invoked" in {
  //  intercept[NoSuchElementException] {
  //    Set.empty.head
  //  }
  //}
}


// vim: set ts=2 sw=2 et sts=2:
