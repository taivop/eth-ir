package ex1

import scala.io.Source

/**
 * Created by taivo on 16/09/15.
 */
object Task1 {

  def readFile(filename: String) = {
    println("Following is the content read:" )
    val text = Source.fromFile(filename).mkString

    println(text)
  }

  def upperCaseStrings(strings: List[String]) = {
    var result = List[Int]()
    for(i <- strings.indices.reverse) {
      if(strings(i).toUpperCase == strings(i))
        result = i :: result
    }

    result
  }

  def upperCaseStrings2(strings: List[String]): List[Int] = {
    strings.zipWithIndex
      .filter { case(x, i) => (x == x.toUpperCase)}
      .map(_._2)
  }

  def sum_reduce(l: Array[Int]) = {
    l.reduceLeft(_ + _)
  }

  def sum_foreach(l: Array[Int]) = {
    var s = 0
    l.foreach(a => s += a)
    s
  }

  def main(args: Array[String]): Unit = {
    readFile("data/test.txt")

    println(upperCaseStrings2("AA" :: "as" :: "AAAA" :: "ASD" :: "ASd" :: Nil))

    println(sum_reduce((1 to 1000).toArray))

    println(sum_foreach((1 to 1000).toArray))
  }

}
