package com.feynmanliang.kws

import java.io.File

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
  def apply(ctmPath: String, obDictPath: String, qDictPath: String): KWSIndexMorph  = {
    val index = KWSIndex(ctmPath)
    val md = MorphDecompose(qDictPath, obDictPath)

    new KWSIndexMorph(index.index, md)
  }

  def main(args: Array[String]):Unit = {
    case class Config(
      ctmFile: File = new File("."),
      queryFile: File = new File("."),
      dict: File = new File("."),
      kwDict: File = new File("."),
      morphDecompose: Boolean = false,
      out: File = new File("."))

    val parser = new scopt.OptionParser[Config]("KWSIndex") {
      head("kwsindex")
      opt[File]('c', "ctmFile") required() valueName("<file>") action { (x, c) =>
      c.copy(ctmFile = x) } text("ctmFile is a required file property")
      opt[File]('q', "queryFile") required() valueName("<file>") action { (x, c) =>
      c.copy(queryFile= x) } text("queryFile is a required file property")
      opt[File]('d', "dict") required() valueName("<file>") action { (x, c) =>
      c.copy(dict= x) } text("dict is a required file property")
      opt[File]('k', "kwDict") required() valueName("<file>") action { (x, c) =>
      c.copy(kwDict= x) } text("kwDict is a required file property")
      opt[File]('o', "out") required() valueName("<file>") action { (x, c) =>
      c.copy(out = x) } text("out is a required file property")
    }
    parser.parse(args, Config()) match {
      case Some(config) =>
        val indexMorph = KWSIndexMorph(
          config.ctmFile.getPath(),
          config.dict.getPath(),
          config.kwDict.getPath())
        val queryResults = indexMorph.kws(config.queryFile.getPath())
        scala.xml.XML.save(config.out.getPath(), queryResults.toXML())
      case None =>
        // arguments are bad, error message will have been displayed
        sys.error("Error parsing arguments!")
    }
  }
}


// vim: set ts=2 sw=2 et sts=2:
