package calculator


object Polynomial {

  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal {
      val valb = b()
      valb * valb - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val delta = computeDelta(a, b, c)()
      delta match {
        case d if d < 0 => Set()
        case d if d >= 0 => {
          val vala = a()
          val valb = b()
          Set() + calcRoot1(vala, valb, delta) + calcRoot2(vala, valb, delta)
        }
      }
    }
  }

  def calcRoot1(a: Double, b: Double, delta: Double):Double = {
    (b * (-1) + Math.sqrt(delta)) / (a * 2)
  }

  def calcRoot2(a: Double, b: Double, delta: Double):Double = {
    (b * (-1) - Math.sqrt(delta)) / (a * 2)
  }
}
