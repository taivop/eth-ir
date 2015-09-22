package ex1

object Task2 {

  // http://stackoverflow.com/questions/8217764/cartesian-product-of-two-lists
  def cartesian_yield[T](xss: List[List[T]]): List[List[T]] = xss match {
    case Nil => List(Nil)
    case h :: t => for(xh <- h; xt <- cartesian_yield(t)) yield xh :: xt
  }

  def cartesian_map[T](xss: List[List[T]]): List[List[T]] = xss match {
    case Nil => List(Nil)
    case h :: t => h.flatMap(xh => xh :: cartesian_map(t)) //for(xh <- h; xt <- cartesian_yield(t)) yield xh :: xt
  }


  def main(args: Array[String]): Unit = {
    println(cartesian_yield(List((1 to 2).toList, (3 to 5).toList)))
  }

}
