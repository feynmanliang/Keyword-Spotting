package com.feynmanliang.kws

object ResultCombiner {
  def combine(qr1: QueryResult, qr2: QueryResult): QueryResult = qr1.copy(
    results=(qr1.results.keys++qr2.results.keys).map {
      k => k -> (qr1.results.getOrElse(k,Set()) ++ qr2.results.getOrElse(k,Set()))
    }.toMap)

  def main(args: Array[String]):Unit = {
    val scoreNorm = "NONE"
    val index = KWSIndex("lib/ctms/decode.ctm", scoreNorm)
    val qr1 = index.kws("lib/kws/queries.xml")

    val indexMorph = KWSIndexMorph(
      ctmPath = "lib/ctms/decode-morph.ctm",
      obDictPath = "lib/dicts/morph.dct",
      qDictPath = "lib/dicts/morph.kwslist.dct",
      scoreNorm = scoreNorm)
    val qr2 = indexMorph.kws("lib/kws/queries.xml")
    scala.xml.XML.save("output/combine.xml", ResultCombiner.combine(qr1, qr2).toXML())
  }
}

// vim: set ts=2 sw=2 et sts=2:
