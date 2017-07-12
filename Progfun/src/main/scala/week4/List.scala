package week4

/**
  * Created by gasdia73 on 13/04/17.
  */
trait List[T]{
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")

}

object List {
  def apply[T](): List[T] = new Nil
  def apply[T](arg: T): List[T] = new Cons(arg, new Nil)
  def apply[T](arg1: T, arg2: T): List[T] = new Cons(arg1, new Cons(arg2, new Nil))
}