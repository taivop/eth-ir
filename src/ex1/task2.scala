package ex1

object Task2 {

  // http://stackoverflow.com/questions/8217764/cartesian-product-of-two-lists
  def cartesianYield[T](xss: List[List[T]]): List[List[T]] = xss match {
    case Nil => List(Nil)
    case h :: t => for(xh <- h; xt <- cartesianYield(t)) yield xh :: xt
  }

  /*def cartesian_map[T](xss: List[List[T]]): List[List[T]] = xss match {
    case Nil => List(Nil)
    case h :: t => h.map(xh => xh :: cartesian_map(t))
  }*/


  def nonEmptiesYield(xss: List[List[String]]): List[Int] = xss match {
    case Nil => Nil
    case ys :: yss => (for(y <- ys if(y.length() > 0)) yield y.length) ++ nonEmptiesYield(yss)
  }

  def nonEmptiesMap(xss: List[List[String]]): List[Int] = {
    xss.flatMap(xs => xs.map(x => x.length()).filter(_ > 0))
  }

  def validInts(xs: List[String]): List[Int] =
    xs.map(a => try {
      Some(a.toInt)
    } catch {
      case _: Exception => None
    }).withFilter(a => a match {
      case None => false
      case Some(a) => true
    }).map(a => a match {case Some(a) => a})

  def main(args: Array[String]): Unit = {
    println(cartesianYield(List((1 to 2).toList, (3 to 5).toList)))

    println(nonEmptiesMap(List(List("a", "", ""), List("asdaaa", "asdaaa"))))
    println(nonEmptiesYield(List(List("a", "", ""), List("asdaaa", "asdaaa"))))

    println(validInts(List("2", "asd", "56", "0")))
  }

}
