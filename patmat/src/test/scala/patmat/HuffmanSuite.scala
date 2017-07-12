package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times test1") {
    val l = List('a', 'b', 'a')
    println(times(l))
    assert(times(l) === List(('a', 2), ('b', 1)) || times(l) === List(('b', 1), ('a', 2)))
  }

  test("numChars test1") {
    val l = List('a', 'b', 'a')
    println("result "+numChars('a', l))
  }

  test("purgeChars test1") {
    val l = List('a', 'b', 'a')
    assert(purgeChar('a', l) === List('b'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("makeOrderedLeafList test 2") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('a', 4), ('f', 8), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3), Leaf('a',4), Leaf('f',8)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine test2") {
    val leaflist = List(Leaf('e', 3), Leaf('t', 4), Leaf('x', 6))
    assert(combine(leaflist) === List(Leaf('x', 6),Fork(Leaf('e',3),Leaf('t',4),List('e', 't'),7)))
  }

  test("combine two elements") {
    val leaflist = List(Leaf('e', 3), Leaf('t', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',3),Leaf('t',4),List('e', 't'),7)))
  }

  test("combine test3") {
    val leaflist = List(Leaf('x', 6),Fork(Leaf('e',3),Leaf('t',4),List('e', 't'),7))
    assert(combine(leaflist) === List(Fork(Leaf('x', 6),Fork(Leaf('e',3),Leaf('t',4),List('e', 't'),7),List('x','e','t'), 13)))
  }

  test("until test1") {
    val leaflist = List(Leaf('e', 3), Leaf('t', 4), Leaf('x', 6))
    assert(until(singleton, combine)(leaflist) === List(Fork(Leaf('x', 6),Fork(Leaf('e',3),Leaf('t',4),List('e', 't'),7),List('x','e','t'), 13)))
  }

  test("createCodeTree test") {
    val l = string2Chars("this is a test")
    val tree = createCodeTree(l)
    println("tree " + tree)

    val bits: List[Bit] = 0 :: 0 :: 0 :: 1 :: Nil
    assert(decode(tree, bits) === List('t', 's'))
  }

  test("createCodeTree test2") {
    val l = string2Chars("abcdef")
    val tree = createCodeTree(l)
    println("tree " + tree)

    println(""+encode(tree)(List('a')))
  }

  test("decode secret") {
    println("secret: "+decodedSecret)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t2, encode(t2)("abd".toList)) === "abd".toList)
    }
  }

  test("convert") {
    new TestTrees {
      println(""+convert(t2))
    }
  }

  test("quickEncode") {
    new TestTrees {
      println(""+quickEncode(t1)("ab".toList))
    }
  }

  test("decode and encode using quickEncode") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("mergeCodeTables test1") {
    val c1 = ('a',0 :: 1 :: Nil) :: ('b',0 :: 1 :: Nil) :: ('c',0 :: 1 :: Nil) :: Nil
    val c2 = ('c',0 :: 1 :: Nil) :: ('e',0 :: 1 :: Nil) :: ('f',0 :: 1 :: Nil) :: Nil
    val m = ('a',0 :: 1 :: Nil) :: ('b',0 :: 1 :: Nil) :: ('c',0 :: 1 :: Nil) :: ('e',0 :: 1 :: Nil) :: ('f',0 :: 1 :: Nil) :: Nil
    assert(mergeCodeTables(c1, c2) === m)
  }

}
