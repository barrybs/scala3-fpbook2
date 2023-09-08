package chapter5

import org.junit.Test
import org.junit.*
import org.junit.Assert.*
import scala.List.*
import chapter5.LazyList.*

class LazyListTest:
  val ll: LazyList[Int] = LazyList(1, 2, 3, 4, 5)
  val l:List[Int] = List(1,2,3,4,5)

  @Test
  def testToList =
    assertEquals(l, ll.toList)
  @Test
  def testTake =
    assertEquals(LazyList(1, 2, 3).toList, take(ll, 3).toList)
    assertEquals(Nil, take(ll, 0).toList)
    assertEquals(List(1), take(ll, 1).toList)
  @Test
  def testDrop =
    assertEquals(LazyList(4,5).toList, drop(ll, 3).toList)

  @Test
  def testTakeWhile =
    assertEquals(LazyList(1,2).toList, takeWhile(ll,_<3).toList)