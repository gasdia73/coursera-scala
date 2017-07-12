package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

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


  property("gen test git push") = forAll { (a: Int, b:Int) =>
    val h = insert(b, insert(a, empty))
    val bigger = findMin(h)
    val smaller = findMin(deleteMin(h))
    bigger >= smaller
  }

//  property("gen1") = forAll { (h: H) =>
//    val m = if (isEmpty(h)) 0 else findMin(h)
//    findMin(insert(m, h)) == m
//  }
//
//  property("gen2") = forAll { (a: Int, b: Int) =>
//    val smaller = ord.min(a, b)
//    val h1 = insert(a, empty)
//    val h2 = insert(b, h1)
//    findMin(h2) == smaller
//  }
//
//  property("gen3") = forAll { (a: Int) =>
//    val h1 = insert(a, empty)
//    val h2 = deleteMin(h1)
//    isEmpty(h2) == true
//  }
//
//  property("gen4") = forAll { (h: H) =>
//    def orderedList(h: H): List[Int] = {
//      if (isEmpty(h)) List()
//      else findMin(h) :: orderedList(deleteMin(h))
//    }
//
//    val l = orderedList(h)
//    l.sorted == l
//  }
//
//  property("gen5") = forAll { (h1: H, h2: H) =>
//    def minOrZero(h: H) = {
//      if (isEmpty(h)) 0 else findMin(h)
//    }
//
//    val hmeld = meld(h1, h2)
//    minOrZero(hmeld) == minOrZero(h1) || minOrZero(hmeld) == minOrZero(h2)
//  }
//



}
