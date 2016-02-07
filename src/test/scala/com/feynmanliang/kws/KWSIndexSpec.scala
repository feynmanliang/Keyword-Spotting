package com.feynmanliang.kws

import scala.io.Source

import org.scalatest.FlatSpec

class KWSIndexSpec extends FlatSpec {

  "A KWSIndex" should "build a KWSIndex with fromFile" in {
    val referenceCtm = Source.fromFile("lib/ctms/reference.ctm")
    val index = KWSIndex.fromFile(referenceCtm)
    assert(!index.get("halo").isEmpty)
    println(index.toXML())
  }

  //it should "produce NoSuchElementException when head is invoked" in {
  //  intercept[NoSuchElementException] {
  //    Set.empty.head
  //  }
  //}
}


// vim: set ts=2 sw=2 et sts=2:
