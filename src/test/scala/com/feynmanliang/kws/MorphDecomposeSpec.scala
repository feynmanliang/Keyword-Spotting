package com.feynmanliang.kws

import org.scalatest.FlatSpec

class MorphDecomposeSpec extends FlatSpec {
  val qPath = "lib/dicts/morph.kwslist.dct"
  val obPath = "lib/dicts/morph.dct"
  val md = MorphDecompose(qPath, obPath)

 "A MorphDecompose" should "decompose a query string" in {
    assert(md.decomposeQuery("walini") === "wali ni")
    assert(md.decomposeQuery("very vile") === "ve ry vi le")
  }

  it should "return the query unchanged if no entry in morph dict" in {
    assert(md.decomposeQuery("abcdefg hijkl") === "abcdefg hijkl")
  }

  it should "decompose a CTMEntry" in {
    val entry = CTMEntry("", 1, 0.5, 0.4, "vile", None, 0.33, 0.09)
    assert(md.decomposeEntry(entry) == List(
      CTMEntry("", 1, 0.5, 0.2, "vi", None, 0.33, 0.3),
      CTMEntry("", 1, 0.7, 0.2, "le", Some("vi"), 0.7, 0.3)
    ))
  }

  it should "return the CTMEntry unchanged if no entry in morph dict" in {
    val entry = CTMEntry("", 1, 0.5, 0.4, "abcdefg", None, 0.33, 0.09)
    assert(md.decomposeEntry(entry) == List(entry))
  }
}

// vim: set ts=2 sw=2 et sts=2:
