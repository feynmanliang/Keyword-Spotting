package com.feynmanliang.kws

import org.scalatest.FlatSpec

class MorphDecomposeSpec extends FlatSpec {
  val qPath = "lib/dicts/morph.kwslist.dct"
  val obPath = "lib/dicts/morph.dct"
  val md = MorphDecompose(qPath, obPath)

  
    //assert(qDict.get("zinacheza").isEmpty)
    //assert(qDict.get("vyovyote").isEmpty)
    //assert(obDict.get("vyovyote").isDefined)
 "A MorphDecompose" should "decompose a query string" in {
    assert(md.decomposeQuery("walini") === "wali ni")
    assert(md.decomposeQuery("very vile") === "ve ry vi le")
  }
}

// vim: set ts=2 sw=2 et sts=2:
