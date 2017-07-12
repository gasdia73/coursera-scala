object rationals {
  val x = new Rational(1, 2)
  val y = new Rational(1, 2)
  val z = new Rational(1, 3)
  x + y
  -x
  y < z
  y max z
  new Rational(2)





  class Rational(x: Int, y: Int) {
    require(y != 0, "denominator must be nonzero")
    private def gcd(a:Int, b:Int):Int = if (b == 0) a else gcd(b, a % b)
    def this(x:Int) = this(x, 1)

    def numer = x
    def denom = y

    def + (that:Rational) =
      new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom
      )

    def unary_- : Rational = new Rational(-numer, denom)

    def - (that:Rational) = this + (-that)

    def < (that:Rational) = numer * that.denom < that.numer * denom

    def max(that:Rational) = if (this < that) that else this



    override def toString = {
      val g = gcd(numer, denom)
      numer/g + "/" + denom/g
    }
  }

}

