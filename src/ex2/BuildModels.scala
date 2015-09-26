package ex2

import scala.collection.mutable.{Map => MutMap}
import scala.io.Source
import java.io._

object buildModels {

  def readFile(filename: String) = {
    Source.fromFile(filename).mkString
  }

  /*def ngrams(doc: Stream[Char], n: Int): Map[String,Int] = {
    if (n<1) Map()
    else {
      val result = MutMap[String,Int]()
      for (ngram <- doc.sliding(n).)
        result(ngram) = result.getOrElse(ngram,0)+1
      result.toMap
    } }*/


  // also make lowercase
  def ngrams(doc: String, n: Int): Map[String,Double] = {
    if (n<1 || doc.length<n) Map()
    else {
      var count = 0

      val result = MutMap[String,Int]()
      for (ngram <- doc.toLowerCase().replaceAll("\\s", " ").replaceAll("[^a-zõäöü -]", "").sliding(n)) {
        result(ngram) = result.getOrElse(ngram,0)+1
        count += 1
      }

      // Create new map for log norm probabilities
      def logNorm(pair: (String, Int)): (String, Double) = pair match { case (s, n) => (s, Math.log(n) - Math.log(count)) }
      val logNormResult = result.toMap.map(logNorm)

      logNormResult

    }
  }

  // Build model of the specigied language
  def buildModel(lang: String): Unit = {
    println(s"$lang\tBuilding language model...")

    val ng = ngrams(readFile(s"data/europarl/corpora/$lang.txt"), 3)
    val pw = new PrintWriter(new File(s"data/europarl/models/$lang.txt" ))
    ng.foreach((pair) => pair match {case (s, d) => pw.write(s"$s\t$d\n")})
    pw.close

    println(s"$lang\tDone.")
  }


  def main(args: Array[String]): Unit = {
    //buildModel("de")
    //buildModel("en")
    buildModel("et")
    //buildModel("fr")
    //buildModel("it")
  }

}
