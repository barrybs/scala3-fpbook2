package chapter5

import chapter5.LazyListF.*
import org.junit.*
import org.junit.Assert.*

import scala.List.*

class LazyListFTest:
  val ll: LazyListF[Int] = LazyListF(1, 2, 3, 4, 5)
  val l:List[Int] = List(1,2,3,4,5)

  @Test
  def testToList =
    assertEquals(l, ll.toList)
  @Test
  def testTake =
    assertEquals(LazyListF(1, 2, 3).toList, take(ll, 3).toList)
    assertEquals(Nil, take(ll, 0).toList)
    assertEquals(List(1), take(ll, 1).toList)
  @Test
  def testDrop =
    assertEquals(LazyListF(4,5).toList, drop(ll, 3).toList)

  @Test
  def testTakeWhile =
    assertEquals(LazyListF(1,2).toList, takeWhile(ll,_<3).toList)
  @Test
  def testExists =
    assertTrue(exists(ll,_<3))
    assertFalse(exists(ll,_>6))

  @Test
  def testExistsFr =
    assertTrue(existsFr(ll, _ < 3))
    assertFalse(existsFr(ll, _ > 6))

  @Test
  def testForAll =
    assertTrue(forAll(ll, _ > 0))
    assertFalse(forAll(ll, _ > 2))
    assertFalse(forAll(ll, _ < 0))
    assertTrue(forAll(ll, _ < 6))

  @Test
  def testForAllFr =
    assertTrue(forAllFr(ll,_>0))
    assertFalse(forAllFr(ll,_>2))
    assertFalse(forAllFr(ll,_<0))
    assertTrue(forAllFr(ll, _ < 6))

  @Test
  def testTakeWhileFr =
    assertEquals(LazyListF(1, 2).toList, takeWhileFr(ll, _ < 3).toList)
    assertEquals(Empty.toList, takeWhileFr(ll, _ < 0).toList)
    assertEquals(LazyListF(1, 2,3,4,5).toList, takeWhileFr(ll, _ > 0).toList)
  @Test
  def testHeadOption =
    assertEquals(Some(1), headOption(ll))
  @Test
  def testMap =
    assertEquals(LazyList(2,3,4,5,6).toList, map(LazyListF(1,2,3,4,5))(_+1).toList)
  @Test
  def testFilter =
    assertEquals(LazyList(1, 3, 4, 5).toList, filter(LazyListF(1, 2, 3, 4, 5))(_ != 2).toList)
  @Test
  def testAppend =
    assertEquals(LazyList(1, 2, 3, 4, 5, 6, 7, 8).toList, append(LazyListF(1, 2, 3, 4, 5),LazyListF(6,7,8)).toList)

  //Non riesco a testare flatMap per un problema di inferenza di tipo. Probabilmente con
  //le versioni delle funzioni implementate come metodi (come nel libro) non si verificano problemi.
  /*@Test
    def testFlatMap =
    assertEquals(LazyList(1,1,2,2,3,3).toList, chapter5.LazyList.flatMap(LazyList(1, 2, 3), (i) => LazyList(i, i)).toList)
    */


