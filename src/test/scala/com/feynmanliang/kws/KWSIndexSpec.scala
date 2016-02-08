package com.feynmanliang.kws

import org.scalatest.FlatSpec

class KWSIndexSpec extends FlatSpec {
  val ctmPath = "lib/ctms/reference.ctm"
  val queryFilePath = "lib/kws/queries.xml"
  val index = KWSIndex(ctmPath)
  val queryResults = index.kws(queryFilePath)

  "A KWSIndex" should "contain words known to be in the CTM" in {
    assert(!index.get("halo").isEmpty)
  }

  it should "be case insensitive" in {
    assert(!index.get("rachael").isEmpty)
    assert(!index.get("kisumu").isEmpty)
    assert(index.get("Wenga fans").size === 1)
  }

  it should "only contain phrase queries when words are <0.5 sec apart" in {
    val tokens = "what she has gone"
    assert(index.get("what she has gone").get.size === 1)
    assert(index.get("pat pat pat").get.size === 1)
  }

  it should "perform KWS when given a queryFile" in {
    val ctmPath = "lib/ctms/reference.ctm"
    val index = KWSIndex(ctmPath)

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
