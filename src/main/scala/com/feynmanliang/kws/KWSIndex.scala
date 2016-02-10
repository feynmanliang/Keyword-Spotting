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
    prevToken: Option[String],
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
    val res = tokens.split("\\s+")
      .map(_.toLowerCase)
      .map(index.get)
    if (res.exists(_.isEmpty)) None
    else Some(
      res.map(_.get).reduceLeft { (acc, x) =>
        (for {
          prevEntry <- acc
          entry <- x if (
            // phrases must belong to same file
            entry.kwFile == prevEntry.kwFile
            && prevEntry.startTime < entry.startTime
            && prevEntry.startTime + prevEntry.duration == entry.prevEndTime // TODO: generalize to morphs?
            && entry.startTime - (prevEntry.startTime + prevEntry.duration) < 0.5)
        } yield {
          prevEntry.copy(
            duration = entry.startTime + entry.duration - prevEntry.startTime,
            token = prevEntry.token ++ " " ++ entry.token,
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
    def line2entry(line: String, prevToken: Option[String], prevEndTime: Double): CTMEntry = {
      val items = line.split("\\s+")
      val startTime = items(2).toDouble
      val entry = CTMEntry(
        kwFile = items(0),
        channel = items(1).toInt,
        startTime = startTime,
        duration = items(3).toDouble,
        token = items(4).toLowerCase,
        prevToken = prevToken,
        prevEndTime = prevEndTime,
        score = items(5).toDouble)
      entry
    }
    def line2endTime(line: String): Double = {
      val items = line.split(" ")
      items(2).toDouble + items(3).toDouble
    }
    val ctmFile = Source.fromFile(ctmPath)
    val index = ctmFile.getLines().map { line =>
      line2entry(line, None, 0D)
    }.foldLeft(List.empty[CTMEntry]) { case (acc, entry) =>
      acc match {
        case Nil =>
          entry.copy(
            prevToken = None,
            prevEndTime = 0D) :: acc
        case prevEntry :: _ =>
          if (prevEntry.kwFile != entry.kwFile) {
            entry.copy(
              prevToken = None,
              prevEndTime = 0D) :: acc
          } else {
            entry.copy(
              prevToken = if (prevEntry.startTime >= entry.startTime) {
                prevEntry.prevToken
              } else {
                Some(prevEntry.token)
              },
              prevEndTime = prevEntry.startTime + prevEntry.duration) :: acc
          }
      }
    }.foldLeft(Map.empty[String, Set[CTMEntry]]) { (acc, entry) =>
      acc + (entry.token -> (acc.getOrElse(entry.token, Set.empty[CTMEntry]) + (entry)))
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
