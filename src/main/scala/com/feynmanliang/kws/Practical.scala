package com.feynmanliang.kws

object Practical {
  def main(args: Array[String]):Unit = {
    val ctmPath = "lib/ctms/reference.ctm"
    val queryFilePath = "lib/kws/queries.xml"

    val index = KWSIndex.fromFile(ctmPath)
    val queryResults = index.kws(queryFilePath)
    //println(queryResults.toXML())
    scala.xml.XML.save("output/reference.xml", queryResults.toXML())
  }
}

// vim: set ts=2 sw=2 et sts=2:
