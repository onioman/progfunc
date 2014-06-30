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
    var example = Fork(
                    Leaf('a',8),
                    Fork(
                      Fork(
                        Leaf('b',3),
                        Fork(
                          Leaf('c', 1),
                          Leaf('d', 1),
                          List('c', 'd'), 2),
                        List('b', 'c', 'd'), 5),
                      Fork(
                        Fork(
                          Leaf('e', 1),
                          Leaf('f', 1),
                          List('e', 'f'), 2),
                        Fork(
                          Leaf('g', 1),
                          Leaf('h', 1),
                          List('g', 'h'), 2),
                        List('e', 'f', 'g', 'h'), 4),
                      List('b', 'c', 'd', 'e', 'f', 'g', 'h'), 9),
                    List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'), 17)
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

  test("time of(\"a\")") {
    assert(List(('a', 1)) === times(string2Chars("a")))
  }

  test("time of(\"aaa\")") {
    assert(List(('a', 3)) === times(string2Chars("aaa")))
  }

  test("time of(\"abcab\")") {
    assert(List(('a', 2), ('b', 2), ('c', 1)) === times(string2Chars("abcab")))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine two leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode 1011 with example") {
    new TestTrees {
      assert(decode(example, List(1,0,1,1)) === "d".toList)
    }
  }

  test("deocde 10001010 with example") {
    new TestTrees {
      assert(decode(example, List(1, 0, 0, 0, 1, 0, 1, 0)) === "bac".toList)
    }
  }

  test("encode a with example") {
    new TestTrees {
      assert(encode(example)(string2Chars("a")) === List(0))
    }
  }
  test("encode ab with example") {
    new TestTrees {
      assert(encode(example)(string2Chars("ab")) === List(0, 1, 0, 0))
    }
  }

  test("encode abh with example") {
    new TestTrees {
      assert(encode(example)(string2Chars("abh")) === List(0, 1, 0, 0, 1, 1, 1, 1))
    }
  }

  test("secrete decode") {
    new TestTrees {
      println(decodedSecret)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("convert example tree") {
    new TestTrees {
      val codeTree = convert(example)
      codeTree foreach println
    }
  }

  test("quick encode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }


}
