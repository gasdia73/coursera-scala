package recfun

object Main {
  def main(args: Array[String]) {
    //    println("Pascal's Triangle")
    //    for (row <- 0 to 10) {
    //      for (col <- 0 to row)
    //        print(pascal(col, row) + " ")
    //      println()
    //    }
//
//    println(mysum(x => x)(3, 4))
//    println(myproduct(x => x)(3, 4))
//    println(factorial(x => x)(4))
    println(factorial(x => x)(5))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if ((c == 0) || (c == r)) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def go(open: Int, chars: List[Char]): Boolean = {
      if (open < 0) false
      else if (chars.isEmpty) true
      else if (chars.head == '(') go(open + 1, chars.tail)
      else if (chars.head == ')') go(open - 1, chars.tail)
      else go(open, chars.tail)
    }

    go(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }


  def mysum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }

    loop(a, 0)
  }

  def myproduct(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * myproduct(f)(a + 1, b)
  }

  def factorial(f:Int => Int)(x: Int): Int =
    myproduct(f)(1, x)


  

}
