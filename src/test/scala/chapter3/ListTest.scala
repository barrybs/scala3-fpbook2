package chapter3

import chapter3.List.*
import org.junit.*
import org.junit.Assert.*

def dTos = {
  dToS
}

class ListTest:
  val l: List[Int] = List(1, 2, 3, 4, 5)
  @Test def testTail =
    assertEquals(List(2, 3, 4, 5), tail(l))
  @Test def testInsert =
    assertEquals(List(0, 1, 2, 3, 4, 5),insert(0, l))
  @Test
  def testDrop =
    assertEquals(List(4, 5), drop(l, 3))
  @Test
  def testDropOversize =
    assertEquals(Nil, drop(l, 6))
  @Test
  def testDropWhile =
    assertEquals(List(3, 4, 5), dropWhile(l, _ < 3) )
  @Test
  def testInit =
    assertEquals(List(1, 2, 3, 4), init(l))
    assertEquals(Nil, init(List(1)))
    assertEquals(Nil, Nil)
  @Test
  def testFoldRightSum =
    assertEquals(15, foldRight(l,0, _+_))
  @Test
  def testFoldRightProd =
    assertEquals(120, foldRight(l, 1, _*_))
  @Test
  def testFoldRightZero =
    assertEquals(0, foldRight(List(1,2,3,0,4,5,6), 1, _*_))
  @Test
  def testFoldRightNilAndCons =
    println(foldRight(List(1,2,3), Nil: List[Int], Cons(_,_)))
  @Test
  def testFoldRightLSum =
    assertEquals(15, foldRightWithFoldLeft1(l, 0, _ + _))
    assertEquals(15, foldRightWithFoldLeft2(l, 0, _ + _))
  @Test
  def testFoldLeftWithFoldRight =
    assertEquals(15, foldLeftWithFoldRight(l, 0, _ + _))
  @Test
  def testLength =
    assertEquals(5, length(l))
    assertEquals(3, length(List(1,2,3)))
  @Test
  def testSumFl =
    assertEquals(15, sumFl(l))
  @Test
  def testProdFl =
    assertEquals(120, prodFl(l))
  @Test
  def testLengthFl =
    assertEquals(5, lengthFl(l))
    assertEquals(3, lengthFl(List(1, 2, 3)))
  @Test
  def testFoldLeftSum =
    assertEquals(15, foldLeft(l, 0, _ + _))
  @Test
  def testFoldLeftProd =
    assertEquals(120, foldLeft(l, 1, _ * _))
  @Test
  def testReverse =
    assertEquals(List(3,2,1), reverse(List(1,2,3)))
    assertEquals(Nil, reverse(Nil))
    assertEquals(List(1), reverse(List(1)))
  @Test
  def testReverseFl =
    assertEquals(List(3, 2, 1), reverseFl(List(1, 2, 3)))
    assertEquals(Nil, reverseFl(Nil))
    assertEquals(List(1), reverse(List(1)))
  @Test
  def testAppend =
    assertEquals(List(1,2,3,4), append(List(1,2),List(3,4)))
  @Test
  def testConcat =
    assertEquals(List(1, 2, 3, 4), concat(List (List(1), List(2), List(3,4)) ))
  @Test
  def testAppendFl =
    assertEquals(List(1,2,3,4), appendFl(List(1,2),List(3,4)))
  @Test
  def testAddOne =
    assertEquals(List(2,3,4,5,6), incrementEach(List(1,2,3,4,5)))
    assertEquals(List(2), incrementEach(List(1)))
    assertEquals(Nil, incrementEach(Nil))
  @Test
  def testAddOneFr =
    assertEquals(List(2, 3, 4, 5, 6), incrementEachFr(List(1, 2, 3, 4, 5)))

  @Test
  def testDToS =
    assertEquals(List("1.0", "2.0", "3.0", "4.0", "5.0"), dTos(List(1, 2, 3, 4, 5)))

  @Test
  def testDToSFr =
    assertEquals(List("1.0", "2.0", "3.0", "4.0", "5.0"), dToSFr(List(1, 2, 3, 4, 5)))

  @Test
  def testMap =
    assertEquals(List("1", "2", "3", "4", "5"), map(List(1, 2, 3, 4, 5), x => x.toString))
    assertEquals(List(2, 3, 4, 5, 6), map(List(1, 2, 3, 4, 5), a => a+1))
    assertEquals(List(1, 4, 9, 16, 25), map(List(1, 2, 3, 4, 5), a => Math.pow(a,2)))

  @Test
  def testFilter =
    assertEquals(List(2, 3, 4, 5), filter(List(1, 2, 3, 4, 5), a => a>1))

  @Test
  def testFilter =
    assertEquals(List(2, 3, 4, 5), filter(List(1, 2, 3, 4, 5), a => a > 1))

  @Test
  def testMyDelete =
    assertEquals(List(1, 2, 4, 5), myDelete(l, _ == 3))
    assertEquals(List(1, 2, 3), myDelete(l, _ > 3))