package com.feynmanliang.kws

import java.io.{BufferedWriter, File, FileWriter}

object LengthMap {
  def main(args: Array[String]):Unit = {
    case class Config(
      queryFile: File = new File("."),
      lengthType: String = "WORD",
      out: File = new File("."))

    val parser = new scopt.OptionParser[Config]("LengthMap") {
      head("lengthmap")
      opt[File]('q', "queryFile") required() valueName("<file>") action { (x, c) =>
      c.copy(queryFile= x) } text("queryFile is a required file property")
      opt[String]('l', "lengthType") required() valueName("<WORD|MORPH>") action { (x, c) =>
      c.copy(lengthType= x) } text("lengthType is a required file property")
      opt[File]('o', "out") required() valueName("<file>") action { (x, c) =>
      c.copy(out = x) } text("out is a required file property")
    }
    // parser.parse returns Option[C]
    parser.parse(args, Config()) match {
      case Some(config) =>
        val md = MorphDecompose("lib/dicts/morph.kwslist.dct", "lib/dicts/morph.dct")

        val file = new File(config.out.getPath())
        val bw = new BufferedWriter(new FileWriter(file))
        // TODO: length = number of morphs instead of number of words?
        val queryFile = scala.xml.XML.loadFile(config.queryFile.getPath())
        (queryFile \ "kw").map { kw =>
          val kwId = (kw \ "@kwid").text.drop(6)
          val length = config.lengthType match {
            case "WORD" => (kw \ "kwtext").text.split("\\s+").size
            case "MORPH" => (kw \ "kwtext").text.split("\\s+").flatMap(md.decomposeQuery).size
          }
          s"${length} ${kwId}\n"
        }
        .foreach(bw.write)
        bw.close()

      case None =>
        // arguments are bad, error message will have been displayed
        sys.error("Error parsing arguments!")
    }
  }
}

// vim: set ts=2 sw=2 et sts=2:
