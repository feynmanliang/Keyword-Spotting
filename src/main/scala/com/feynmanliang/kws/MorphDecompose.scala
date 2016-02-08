package com.feynmanliang.kws

import scala.io.Source

class KWSIndexMorph(
    index: Map[String, Set[CTMEntry]],
    md: MorphDecompose) extends KWSIndex(index) {
  val morphIndex = index.mapValues(_.flatMap(md.decomposeEntry))
  override def get(tokens: String): Option[Set[CTMEntry]] = {
    val res = tokens.split("\\s+")
      .map(_.toLowerCase)
      .flatMap(md.decomposeQuery(_).split("\\s+"))
      .flatMap(index.get)
    if (res.isEmpty) None
    else Some(
      res.reduceLeft { (acc, x) =>
        (for {
          prevEntry <- acc
          entry <- x if (
            entry.kwFile == prevEntry.kwFile
            && prevEntry.startTime < entry.startTime
            && prevEntry.startTime + prevEntry.duration == entry.prevEndTime
            && entry.startTime - (prevEntry.startTime + prevEntry.duration) < 0.5)
        } yield {
          entry.copy(
            startTime = prevEntry.startTime,
            duration = entry.startTime + entry.duration - prevEntry.startTime,
            token = prevEntry.token ++ " " ++ entry.token,
            prevEndTime = prevEntry.prevEndTime,
            score = prevEntry.score * entry.score
          )
        }).toSet
      })
  }
}

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
      case None => sys.error(s"Couldn't morph decompose token: ${entry.token}")
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
