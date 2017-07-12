package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.collection.immutable.Set

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      i <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield {
      println("insert " + i + " in " + h)
      insert(i, h)
    }
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  property("all numbers from list should be in the tree") = forAll { (l: List[Int]) =>
    val ldistinct = l.distinct
    val tree = ldistinct.foldLeft(empty)((h, a) => insert(a, h))

    def check(s: Set[Int], h: H): Boolean = {
      if (isEmpty(h)) {
        if (s.size == ldistinct.size) true
        else false
      }
      else check(s + findMin(h), deleteMin(h))
    }

    check(Set(), tree)
  }

  property("after inserting and deleting random numbers the tree should be empty") = forAll { (l: List[Int]) =>
    val tree = l.foldLeft(empty)((h, a) => insert(a, h))

    def delTree(count: Int, h: H): H = {
      //      println("count "+count+" h "+ h)
      if (count.equals(0)) h
      else delTree(count - 1, deleteMin(h))
    }

    isEmpty(delTree(l.size, tree))
  }

  property("gen1") = forAll { (a: Int) =>
    val tree = insert(a, insert(a, empty))
    val deltree = deleteMin(deleteMin(tree))
    isEmpty(deltree)
  }

  property("gen1111") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (a: Int, b: Int) =>
    val smaller = ord.min(a, b)
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == smaller
  }

  property("gen3") = forAll { (a: Int) =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2) == true
  }

  property("gen4") = forAll { (h: H) =>
    def orderedList(tr: H): List[Int] = {
      if (isEmpty(tr)) List()
      else findMin(tr) :: orderedList(deleteMin(tr))
    }

    val l = orderedList(h)
    l.sorted == l
  }

  property("gen5") = forAll { (h1: H, h2: H) =>
    def minOrZero(h: H) = {
      if (isEmpty(h)) 0 else findMin(h)
    }

    val hmeld = meld(h1, h2)
    minOrZero(hmeld) == minOrZero(h1) || minOrZero(hmeld) == minOrZero(h2)
  }


}
