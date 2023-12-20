package chapter5

import chapter5.LazyList.*
import org.junit.Assert.*
import org.junit.*

class LazyListTest:
  val ll: LazyList[Int] = LazyList(1, 2, 3, 4, 5)
  val l:List[Int] = List(1,2,3,4,5)

  @Test
  def testToList =
    assertEquals(l, ll.toList)
  @Test
  def testTake =
    assertEquals(LazyList(1, 2, 3).toList, ll.take(3).toList)
    assertEquals(Nil, ll.take(0).toList)
    assertEquals(List(1), ll.take(1).toList)
  @Test
  def testDrop =
    assertEquals(LazyList(4,5).toList, ll.drop(3).toList)

  @Test
  def testTakeWhile =
    assertEquals(LazyList(1,2).toList, ll.takeWhile(_<3).toList)
  @Test
  def testExists =
    assertTrue(ll.exists(_<3))
    assertFalse(ll.exists(_>6))

  @Test
  def testExistsFr =
    assertTrue(ll.existsFr(_ < 3))
    assertFalse(ll.existsFr(_ > 6))

  @Test
  def testForAll =
    assertTrue(ll.forAll(_ > 0))
    assertFalse(ll.forAll(_ > 2))
    assertFalse(ll.forAll(_ < 0))
    assertTrue(ll.forAll(_ < 6))

  @Test
  def testForAllFr =
    assertTrue(ll.forAllFr(_>0))
    assertFalse(ll.forAllFr(_>2))
    assertFalse(ll.forAllFr(_<0))
    assertTrue(ll.forAllFr(_ < 6))

  @Test
  def testTakeWhileFr =
    assertEquals(LazyList(1, 2).toList, ll.takeWhileFr(_ < 3).toList)
    assertEquals(Empty.toList, ll.takeWhileFr(_ < 0).toList)
    assertEquals(LazyList(1, 2,3,4,5).toList, ll.takeWhileFr(_ > 0).toList)
  @Test
  def testHeadOption =
    assertEquals(Some(1), ll.headOption)
  @Test
  def testMap =
    assertEquals(LazyList(2,3,4,5,6).toList, ll.map(_+1).toList)
  @Test
  def testFilter =
    assertEquals(LazyList(1, 3, 4, 5).toList, ll.filter(_ != 2).toList)
  @Test
  def testAppend =
    assertEquals(LazyList(1, 2, 3, 4, 5, 6, 7, 8).toList, ll.append(LazyList(6,7,8)).toList)

  //Non riesco a testare flatMap per un problema di inferenza di tipo. Probabilmente con
  //le versioni delle funzioni implementate come metodi (come nel libro) non si verificano problemi.
  @Test
  def testFlatMap =
    assertEquals(LazyList(1,1,2,2,3,3).toList, LazyList(1, 2, 3).flatMap(i => LazyList(i, i)).toList)

  val ones: LazyList[Int] = LazyList.cons(1, ones)
  val twos: LazyList[Int] = continually(2)
  @Test
  def testInfiniteList = 
    println(ones.exists(_ %2 !=0))
    println(ones.map(_+1).exists(_%2 ==0))
    println(ones.takeWhile(_ == 1))
    println(ones.forAll(_ != 1))

    println(twos.exists(_ % 2 == 0))
    println(twos.map(_ + 1).exists(_ % 2 != 0))
    println(twos.takeWhile(_ == 2))
    println(twos.forAll(_ != 2))
  @Test
  def testContinually1 =
    assertEquals(ones.takeWhile(_ == 2).toList, continually1(1).takeWhile(_ == 2).toList)
    assertEquals(ones.takeWhile(_ == 2).toListTr, continually1(1).takeWhile(_ == 2).toListTr)
  @Test
  def testContinually =
    assertEquals(ones.takeWhile(_==2).toList, continually(1).takeWhile(_==2).toList)
    assertEquals(ones.takeWhile(_==2).toListTr, continually(1).takeWhile(_==2).toListTr)

  @Test
  def testFrom =
    assertEquals(List(1,2,3,4,5,6,7,8,9,10), from(1).takeWhile(_<=10).toListTr)

  @Test
  def testFibs =
    assertEquals(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89), fibs.take(12).toListTr)

  @Test
  def testUnfold =
    assertEquals(List(1,1,1), unfold(1)(_=> Some(1,1)).take(3).toList)
    assertEquals(List(1,2,3), unfold(1)(n=> Some(n,n+1)).take(3).toList)
    assertEquals(List("","a","aa"), unfold("")(s=> Some(s,s+"a")).take(3).toList)

  @Test
  def testFibUnf =
    assertEquals(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89), fibsUnf.take(12).toListTr)

  @Test
  def testFromUnf =
    assertEquals(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), fromUnf(1).takeWhile(_ <= 10).toListTr)

  @Test
  def testContinuallyUnf =
    assertEquals(ones.takeWhile(_ == 2).toList, continuallyUnf(1).takeWhile(_ == 2).toList)
    assertEquals(ones.takeWhile(_ == 2).toListTr, continuallyUnf(1).takeWhile(_ == 2).toListTr)

  @Test
  def testOnesUnf =
    assertEquals(ones.takeWhile(_ == 2).toList, onesUnf.takeWhile(_ == 2).toList)
    assertEquals(ones.takeWhile(_ == 2).toListTr, onesUnf.takeWhile(_ == 2).toListTr)

  @Test
  def testMapUnf =
    assertEquals(LazyList(2,3,4,5,6).toList, mapUnf(ll)(_+1).toList)

  @Test
  def testTakeUnf =
    assertEquals(LazyList(1, 2, 3).toList, takeUnf(ll, 3).toList)
   // assertEquals(Nil, takeUnf(ll, 0).toList)
    //assertEquals(List(1), takeUnf(ll, 1).toList)

  @Test
  def testTakeWhileUnf =
    assertEquals(LazyList(1, 2).toList, takeWhileUnf(ll, _ < 3).toList)

  @Test
  def testZipWithUnf =
  //assertEquals(List(5, 7, 9), combine2(List(1, 2, 3), List(4, 5, 6), _ + _))
    assertEquals(List(5, 7), zipWithUnf(LazyList(1, 2), LazyList(4, 5, 6), _ + _).toList)

  @Test
  def zipAll =
    assertEquals(List( (Some(1),Some(4)), (Some(2), Some(5)), (None, Some(6))), zipAllUnf(LazyList(1, 2), LazyList(4, 5, 6)).toList)

/*  @Test
  def testStartsWith =
    assertEquals(true, startsWith(ll, LazyList(1,2,3)).toList)

*/