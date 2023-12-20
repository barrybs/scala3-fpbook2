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
  @Test def testHead =
    assertEquals(List(1), head(l))

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
    assertEquals(List(List(1,1), List(2,2), List(3,3)), map(List(1, 2, 3), i=>List(i,i)))

  @Test
  def testFilter =
    assertEquals(List(2, 3, 4, 5), filter(List(1, 2, 3, 4, 5), a => a>1))

  @Test
  def testFlatMap =
    assertEquals(List(1,1,2,2,3,3), flatMap(List(1, 2, 3), i => List(i,i)))

  @Test
  def testFlatMap2 =
    assertEquals(List(1, 1, 2, 2, 3, 3), flatMap2(List(1, 2, 3), i => List(i, i)))
  @Test
  def testFilter2 =
    assertEquals(List(2, 3, 4, 5), filter2(List(1, 2, 3, 4, 5), a => a > 1))
  @Test
  def testAddPairWise1 =
    assertEquals(List(5, 7, 9), addPairWise1(List(1,2,3), List(4,5,6) ))
    assertEquals(List(4, 5, 6), addPairWise1(Nil, List(4,5,6) ))
    assertEquals(List(5, 7, 6), addPairWise1(List(1,2), List(4, 5, 6)))
    assertEquals(Nil, addPairWise1(Nil, Nil ))
  @Test
  def testAddPairWise2 =
    assertEquals(List(5, 7, 9), addPairWise2(List(1, 2, 3), List(4, 5, 6)))
    assertEquals(List(5, 7, 6), addPairWise2(List(1,2), List(4, 5, 6)))
    assertEquals(Nil, addPairWise2(Nil, List(4,5,6) ))
  @Test
  def testCombine =
    assertEquals(List(5, 7, 9), combine(List(1,2,3), List(4,5,6), _+_))
    assertEquals(List(4, 5, 6), combine(Nil, List(4,5,6), _+_))
  @Test
  def testZipWith =
    //assertEquals(List(5, 7, 9), combine2(List(1, 2, 3), List(4, 5, 6), _ + _))
    assertEquals(List(5, 7), zipWith(List(1, 2), List(4, 5, 6), _ + _))

  @Test
  def testZipWithTr =
  //assertEquals(List(5, 7, 9), combine2(List(1, 2, 3), List(4, 5, 6), _ + _))
    assertEquals(List(5, 7), zipWithTr(List(1, 2), List(4, 5, 6), _ + _))
  @Test
  def testMyDelete =
    assertEquals(List(1, 2, 4, 5), myDelete(l, _ == 3))
    assertEquals(List(1, 2, 3), myDelete(l, _ > 3))

  @Test
  def testHasSubSequence =
    assertEquals(true, hasSubSequence(List(1,2,3,4,5), List(1,2,3,4,5)))
    assertEquals(true, hasSubSequence(List(1,2,3,4,5), List(2,3)))
    assertEquals(false, hasSubSequence(List(1,2,3,4,5), List(6,7,8)))
    assertEquals(false, hasSubSequence(List(1,2,3,4,5), Nil))
    assertEquals(false, hasSubSequence(List(1,2), List(1,2,3,4,5)))
    assertEquals(false, hasSubSequence(Nil, List(1,2,3,4,5)))
    assertEquals(false, hasSubSequence(List(1), List(2)))
    assertEquals(true, hasSubSequence(List(1), List(1)))
    assertEquals(true, hasSubSequence(List(1,2,1), List(1)))
    assertEquals(true, hasSubSequence(List(0,0,1), List(1)))
    assertEquals(true, hasSubSequence(List(1,1,1), List(1)))
    assertEquals(false, hasSubSequence(Nil, Nil))

  @Test
  def testHasSubSequenceBook =
    assertEquals(true, hasSubSequenceBook(List(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5)))
    assertEquals(true, hasSubSequenceBook(List(1, 2, 3, 4, 5), List(2, 3)))
    assertEquals(false, hasSubSequenceBook(List(1, 2, 3, 4, 5), List(6, 7, 8)))
//    assertEquals(false, hasSubSequenceBook(List(1, 2, 3, 4, 5), Nil))
    assertEquals(false, hasSubSequenceBook(List(1, 2), List(1, 2, 3, 4, 5)))
    assertEquals(false, hasSubSequenceBook(Nil, List(1, 2, 3, 4, 5)))
    assertEquals(false, hasSubSequenceBook(List(1), List(2)))
    assertEquals(true, hasSubSequenceBook(List(1), List(1)))
    assertEquals(true, hasSubSequenceBook(List(1, 2, 1), List(1)))
    assertEquals(true, hasSubSequenceBook(List(0, 0, 1), List(1)))
    assertEquals(true, hasSubSequenceBook(List(1, 1, 1), List(1)))
    //assertEquals(false, hasSubSequenceBook(Nil, Nil))

  @Test
  def testTake =
    assertEquals(List(1, 2), take(l, 2))
  @Test
  def testTakeWhile =
    assertEquals(List(1,2,3), takeWhile(l, _<4) )
  @Test
  def testForAll1 =
    assertEquals(true, forAll1(List(1,2,3),_>0))
    assertEquals(false, forAll1(List(1,2,3),_>2))
  @Test
  def testForAll2 =
    assertEquals(true, forAll2(List(1, 2, 3), _ > 0))
    assertEquals(false, forAll2(List(1, 2, 3), _ > 2))
  @Test
  def testForAllTr =
    assertEquals(true, forAllTr(List(1, 2, 3), _ > 0))
    assertEquals(false, forAllTr(List(1, 2, 3), _ > 2))

  @Test
  def testForAllFr =
    assertEquals(true, forAllFr(List(1, 2, 3), _ > 0))
    assertEquals(false, forAllFr(List(1, 2, 3), _ > 2))
  @Test
  def testForAllFl =
    assertEquals(true, forAllFl(List(1, 2, 3), _ > 0))
    assertEquals(false, forAllFl(List(1, 2, 3), _ > 2))
  @Test
  def testExists1 =
    assertEquals(true, exists1(List(1, 2, 3), _ > 1))
    assertEquals(true, exists1(List(1, 2, 3), _ > 0))
    assertEquals(true, exists1(List(1, 2, 3), _ == 2))
    assertEquals(false, exists1(List(1, 2, 3), _ > 3))

  @Test
  def testExists2 =
    assertEquals(true, exists2(List(1, 2, 3), _ > 1))
    assertEquals(true, exists2(List(1, 2, 3), _ > 0))
    assertEquals(true, exists2(List(1, 2, 3), _ == 2))
    assertEquals(false, exists2(List(1, 2, 3), _ > 3))

  @Test
  def testExists =
    assertEquals(true, exists(List(1, 2, 3), _ > 1))
    assertEquals(true, exists(List(1, 2, 3), _ > 0))
    assertEquals(true, exists(List(1, 2, 3), _ == 2))
    assertEquals(false, exists(List(1, 2, 3), _ > 3))

  @Test
  def testZipRight(): Unit =
    assertEquals(List( ("zero", 0), ("uno", 1), ("due", 2), ("tre", 3), ("quattro", 4)), zipRight(List("zero","uno","due","tre","quattro")))

  @Test
  def testMean() =
    assertEquals(2.5, mean(List(1.0,2.0,3.0,4.0)),0)

  @Test
  def testMaxInt() =
    assertEquals(23, maxInt(List(1, 16, 4, 0, 23, 9)), 0)

  @Test
  def testMaxFl() =
    assertEquals(23, maxIntFL(List(1, 16, 4, 0, 23, 9)), 0)