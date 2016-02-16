package com.feynmanliang.kws

import java.io.File

class KWSIndexMorph(
    index: Map[String, Set[CTMEntry]],
    md: MorphDecompose) extends KWSIndex(index) {

  // Should be true if using decode-morph.ctm
  assert(index.values.reduce(_++_).filter(entry => md.decomposeEntry(entry).size > 1).size == 0)

  //val morphIndex = index.values
  //  .flatMap(_.flatMap(md.decomposeEntry))
  //  .foldLeft(Map[String, Set[CTMEntry]]()) { (acc, x) =>
  //    acc + (x.token -> (acc.getOrElse(x.token, Set[CTMEntry]()) + x))
  //  }
  val morphIndex = index

  override def get(tokens: String): Option[Set[CTMEntry]] = {
    val res = tokens.split("\\s+")
      .map(_.toLowerCase)
      .flatMap(md.decomposeQuery(_))
      .map(morphIndex.get)
    if (res.exists(_.isEmpty)) None
    else Some(
      // TODO: split up by words, then do morph decomposition?
      res.map(_.get).reduceLeft { (acc, x) =>
        (for {
          prevEntry <- acc
          entry <- x
          if (
            prevEntry.kwFile == entry.kwFile
            && prevEntry.startTime < entry.startTime //&& entry.startTime < (prevEntry.startTime + prevEntry.duration) + 0.5
            //&& (entry.prevToken.isEmpty || (entry.prevToken.get == md.decomposeEntry(prevEntry).last.token))
            //&& prevEntry.startTime + prevEntry.duration == entry.prevEndTime
            && true)
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
          ctmPath = config.ctmFile.getPath(),
          obDictPath = config.dict.getPath(),
          qDictPath = config.kwDict.getPath())
        val queryResults = indexMorph.kws(config.queryFile.getPath())
        scala.xml.XML.save(config.out.getPath(), queryResults.toXML())
      case None =>
        // arguments are bad, error message will have been displayed
        sys.error("Error parsing arguments!")
    }
  }
}


// vim: set ts=2 sw=2 et sts=2:
