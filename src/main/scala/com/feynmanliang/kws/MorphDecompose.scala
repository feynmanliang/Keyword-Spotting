package com.feynmanliang.kws

import scala.io.Source

class MorphDecompose private (
    qDict: Map[String, List[String]], obDict: Map[String, List[String]]) {
  def decomposeQuery(tokens: String): String = {
    tokens.split("\\s+")
      .map(_.toLowerCase)
      .flatMap { token => qDict.getOrElse(token, List(token)) }
      .mkString(" ")
  }

  /** Decompose a CTM entry into a list of morphemes
      We distribute startTime/durations and scores uniformly across all morphemes.
   */
  def decomposeEntry(entry: CTMEntry): List[CTMEntry] = {
    obDict.get(entry.token) match {
      case None => List(entry)
      case Some(morphs) => {
        val morphDur = (entry.duration / morphs.size)
        val morphScore = math.pow(entry.score, 1D/morphs.size)
        morphs.zipWithIndex.map { case (morph, i) =>
          val prevEndTime = if (i == 0) entry.prevEndTime else entry.startTime + i*morphDur
          entry.copy(
            startTime = entry.startTime + i*morphDur,
            duration = morphDur,
            token = morph,
            prevEndTime = prevEndTime,
            score = morphScore
          )
        }
      }
    }
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
}

// vim: set ts=2 sw=2 et sts=2:
