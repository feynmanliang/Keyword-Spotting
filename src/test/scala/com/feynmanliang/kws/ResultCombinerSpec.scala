package com.feynmanliang.kws

import org.scalatest.FlatSpec

class ResultCombinerSpec extends FlatSpec {
  val kwsResultPaths = List(
    "lib/kws/morph.xml",
    "lib/kws/word.xml"
  )

  "A ResultsCombiner" should "be buildable from XML kws outputs" in {
    val sn = "NONE"
    val combinedResults = kwsResultPaths
      .map { path =>
        QueryResult.fromXML(scala.xml.XML.load(path), sn)
      }
      .reduceLeft(ResultCombiner.combine)

    assert((combinedResults.toXML() \ "detected_kwlist").length > 100)
    assert(combinedResults.results.values.reduce(_ ++ _) >= combinedResults.results.size)
  }
}


// vim: set ts=2 sw=2 et sts=2:
