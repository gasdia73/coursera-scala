package week5.idealized.scala

/**
  * Created by gasdia73 on 22/04/17.
  */
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = True

  def predecessor: Nat = throw new IllegalArgumentException

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat = that

  def -(that: Nat): Nat = that
}

class Succ(n: Nat) extends Nat {
  def isZero: Boolean = False

  def predecessor: Nat = if(n.isZero)

  def successor: Nat = ???

  def +(that: Nat): Nat = ???

  def -(that: Nat): Nat = ???
}


