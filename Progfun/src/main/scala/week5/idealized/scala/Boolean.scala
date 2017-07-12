package week5.idealized.scala

/**
  * Created by gasdia73 on 21/04/17.
  */
abstract class Boolean {
  def ifThenElse[T](t: =>T, e: =>T): T

  def && (x: => Boolean) = ifThenElse(x, False)
  def || (x: => Boolean) = ifThenElse(True, x)
  def unary_! = ifThenElse(False, True)

  def ==(x: Boolean) = ifThenElse(x, x.unary_!)
  def !=(x: Boolean) = ifThenElse(x.unary_!, x)

  def <(x: Boolean) = ifThenElse(False, x)

}

