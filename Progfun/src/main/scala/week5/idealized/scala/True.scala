package week5.idealized.scala

object True extends Boolean {
  def ifThenElse[T](t: =>T, e: =>T): T = t
}