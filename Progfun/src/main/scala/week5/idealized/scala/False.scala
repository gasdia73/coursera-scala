package week5.idealized.scala

object False extends Boolean {
  def ifThenElse[T](t: =>T, e: =>T): T = e
}