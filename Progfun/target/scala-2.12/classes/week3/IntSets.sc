
object IntSets {

  abstract class IntSet {
    def contains(x: Int): Boolean

    def incl(x: Int): IntSet

    def union(that: IntSet): IntSet
  }

  object Empty extends IntSet {
    def contains(x: Int): Boolean = false

    def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

    def union(that: IntSet) = that

    override def toString: String = "."

  }


  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains(x: Int): Boolean = {
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true
    }

    def incl(x: Int): IntSet = {
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this
    }

    override def toString: String = "{" + left + elem + right + "}"

    def union(that: IntSet): IntSet =
      ((left union right) union that) incl elem

  }

  val t1 = new NonEmpty(3, Empty, Empty)
  val t2 = t1 incl 4

}