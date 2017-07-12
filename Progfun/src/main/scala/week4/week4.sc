import week4._

object week4tests {

  def nth(n: Int, l:List[Int]) = {
    def go(n: Int, acc: Int, l:List[Int]):Int = {
      if (n<0 || l.isEmpty) throw new IndexOutOfBoundsException
      else if (acc == n) l.head
      else go(n, acc+1, l.tail)
    }
    go(n, 0, l)
  }

  val li = new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Nil))))

  nth(3, li)
  



}