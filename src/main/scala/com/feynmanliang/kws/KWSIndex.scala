package com.feynmanliang.kws

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source
import scala.xml.Elem

case class CTMEntry(
    kwFile: String,
    channel: Int,
    startTime: Double,
    duration: Double,
    token: String,
    prevEndTime: Double, // for testing contiguity during phrase query
    score: Double) extends Ordered[CTMEntry] {
  import scala.math.Ordered.orderingToOrdered

  def compare(that: CTMEntry): Int =
    (this.kwFile, this.startTime) compare (that.kwFile, that.startTime)

  def toXML(): Elem = {
    <kw
      file={kwFile}
      channel={f"$channel"}
      tbeg={f"$startTime%.2f"}
      dur={f"$duration%.2f"}
      score={f"$score%.6f"}
      decision="YES" />
  }
}

class KWSIndex(val index: Map[String, Set[CTMEntry]]) {
  def get(tokens: String): Option[Set[CTMEntry]] = {
    val res = tokens.split(" ")
      .map(_.toLowerCase)
      .flatMap(index.get)
    if (res.isEmpty) None
    else Some(
      res.reduceLeft { (acc, x) =>
        (for {
          prevEntry <- acc
          entry <- x if (
            // phrases must belong to same file
            entry.kwFile == prevEntry.kwFile
            && prevEntry.startTime < entry.startTime
            && prevEntry.startTime + prevEntry.duration == entry.prevEndTime // TODO: generalize to morphs?
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

  def kws(queryFilePath: String): QueryResult = {
    val queryFile = scala.xml.XML.loadFile(queryFilePath)
    val results = (queryFile \ "kw").map { kw =>
      this.get((kw \ "kwtext").text) match {
        case None => (kw \ "@kwid").text -> Set()
        case Some(hits) => (kw \ "@kwid").text -> hits
      }
    }.toMap.asInstanceOf[Map[String, Set[CTMEntry]]]

    new QueryResult(queryFilePath.split("/").last, results)
  }


  def write(ctmPath: String): Unit = {
    def entry2line(entry: CTMEntry): String = {
      s"${entry.kwFile} ${entry.channel} ${entry.startTime} ${entry.duration} " +
      s"${entry.token} ${entry.score}"
    }
    val file = new File(ctmPath)
    val bw = new BufferedWriter(new FileWriter(file))
    index.values.toList.flatten.sorted.foreach { entry =>
      bw.write(entry2line(entry))
    }
    bw.close()
  }
}

object KWSIndex {
  def apply(ctmPath: String): KWSIndex = {
    def line2entry(line: String, prevEndTime: Double): CTMEntry = {
      val items = line.split("\\s+")
      val startTime = items(2).toDouble
      //val prevEndTimeTruncate = math.min(prevEndTime, startTime)
      val prevEndTimeTruncate = prevEndTime
      val entry = CTMEntry(
        kwFile = items(0),
        channel = items(1).toInt,
        startTime = startTime,
        duration = items(3).toDouble,
        token = items(4).toLowerCase,
        prevEndTime = prevEndTimeTruncate,
        score = items(5).toDouble)
      entry
    }
    def line2endTime(line: String): Double = {
      val items = line.split(" ")
      items(2).toDouble + items(3).toDouble
    }
    val ctmFile = Source.fromFile(ctmPath)
    val index = ctmFile.getLines().sliding(2).zipWithIndex.flatMap { case (lines,i) =>
      val prevItems = lines(0).split("\\s+")
      val items = lines(1).split("\\s+")
      if (i == 0) {
        val firstEntry = line2entry(lines(1), 0D)
        val entry = line2entry(lines(1), line2endTime(lines(0)))
        List(
          firstEntry.token -> firstEntry,
          entry.token -> entry)
      } else if (prevItems(0) != items(0)) { // new kwFile
        val entry = line2entry(lines(1), 0D)
        List(entry.token -> entry)
      } else { // same kwFile, accumulate endTimes
        val entry = line2entry(lines(1), line2endTime(lines(0)))
        List(entry.token -> entry)
      }
    }.foldLeft(Map.empty[String, Set[CTMEntry]]) { (acc, pair) =>
      acc + (pair._1 -> (acc.getOrElse(pair._1, Set.empty[CTMEntry]) + (pair._2)))
    }
    new KWSIndex(index)
  }

  def main(args: Array[String]):Unit = {
    case class Config(
      ctmFile: File = new File("."),
      queryFile: File = new File("."),
      morphDecompose: Boolean = false,
      out: File = new File("."))

    val parser = new scopt.OptionParser[Config]("KWSIndex") {
      head("kwsindex")
      opt[File]('c', "ctmFile") required() valueName("<file>") action { (x, c) =>
      c.copy(ctmFile = x) } text("ctmFile is a required file property")
      opt[File]('q', "queryFile") required() valueName("<file>") action { (x, c) =>
      c.copy(queryFile= x) } text("queryFile is a required file property")
      opt[File]('o', "out") required() valueName("<file>") action { (x, c) =>
      c.copy(out = x) } text("out is a required file property")
    }
    // parser.parse returns Option[C]
    parser.parse(args, Config()) match {
      case Some(config) =>
        // do stuff
        val index = KWSIndex(config.ctmFile.getPath())
        val queryResults = index.kws(config.queryFile.getPath())
        scala.xml.XML.save(config.out.getPath(), queryResults.toXML())
      case None =>
        // arguments are bad, error message will have been displayed
        sys.error("Error parsing arguments!")
    }
  }
}

// vim: set ts=2 sw=2 et sts=2:
