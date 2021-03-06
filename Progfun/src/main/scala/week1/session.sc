object session {
  def abs(x: Double): Double = if (x < 0) -x else x

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =

      if (isGoodEnough(guess)) guess
      else
        sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)

  }

  def factorial(n: Int): Int =
    if (n == 0) 1 else n * factorial(n - 1)


  def factorial2(n: Int): Int = {
    def go(m: Int, n: Int): Int = {
      if (n == 0) m else
        go(m * n, n-1)
    }
    go(1, n)
  }

  factorial(5)
  factorial2(5)

  sqrt(2)

  sqrt(4)
  sqrt(1e-6)


}