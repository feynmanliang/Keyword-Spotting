package com.feynmanliang.kws

import scala.io.Source

class MorphDecompose private (
    qDict: Map[String, List[String]], obDict: Map[String, List[String]]) {
  def decomposeQuery(tokens: String): String = {
    val res = tokens.split(" ")
      .map(_.toLowerCase)
      .flatMap(qDict.get)
      .flatten
      .mkString(" ")
    res
  }

  /** Decompose a CTM entry into a list of morphemes
      We distribute startTime/durations and scores uniformly across all morphemes.
   */
  def decomposeEntry(entry: CTMEntry): List[CTMEntry] = {
    ???
  }
}

object MorphDecompose {
  private def parseMorphDict(dictPath: String): Map[String, List[String]] = {
    Source.fromFile(dictPath).getLines().map { line =>
      val words = line.split("\\s+")
      words.head -> words.tail.toList
    }.toMap
  }

  def apply(qDictPath: String, obDictPath: String) = {
    val qDict = parseMorphDict(qDictPath)
    val obDict = parseMorphDict(obDictPath)
    new MorphDecompose(qDict, obDict)
  }

  def main(args: Array[String]):Unit = {
    val queryTermMorphs = "lib/dicts/morph.kwslist.dct"
    val oneBestListMorphs= "lib/dicts/morph.dct"

    println(parseMorphDict(queryTermMorphs))
    println(parseMorphDict(oneBestListMorphs))
  }
}

// vim: set ts=2 sw=2 et sts=2:
