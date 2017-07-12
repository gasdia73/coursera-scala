package week5.idealized.scala

/**
  * Created by gasdia73 on 22/04/17.
  */
class ListTest {
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => x :: Nil
    case y :: ys => if (x < y) x :: insert(y, ys) else y :: insert(x, ys)
  }

  def iSort(xs:List[Int]):List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, iSort(ys))
  }

}
