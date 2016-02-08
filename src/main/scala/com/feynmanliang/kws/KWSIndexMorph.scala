package com.feynmanliang.kws

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

object KWSIndexMorph {
  def apply(
      ctmPath: String, qPath: String, obDictPath: String, qDictPath: String): KWSIndexMorph  = {
    val index = KWSIndex(ctmPath)
    val md = MorphDecompose(qDictPath, obDictPath)

    new KWSIndexMorph(index.index, md)
  }
}


// vim: set ts=2 sw=2 et sts=2:
