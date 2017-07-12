object test {

  def squareList1(xs: List[Int]): List[Int] =
    xs match {
      case Nil => xs
      case y :: ys => (y * y) :: squareList1(ys)
    }

//  def squareList(xs: List[Int]): List[Int] =
//    xs.map(x:Int => x*x)


  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => xs.takeWhile(a => a.equals(x)) :: pack(xs1.dropWhile(a => a.equals(x)))
  }

  pack(List("a", "a", "a", "b", "c", "c", "a"))

  def encode[T](xs: List[T]): List[(T, Int)] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      (x, xs.takeWhile(a => a.equals(x)).size) :: encode(xs1.dropWhile(a => a.equals(x)))
  }

  encode(List("a", "a", "a", "b", "c", "c", "a"))
}