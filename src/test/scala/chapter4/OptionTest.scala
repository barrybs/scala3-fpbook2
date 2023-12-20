package chapter4

import scala.collection.immutable.List
import chapter4.Option.*
import org.junit.*
import org.junit.Assert.*

class OptionTest:
  val optInt:Option[Int] = Some(1)
  val optInt2:Option[Int] = Some(2)
  val optString:Option[String] = Some("Ciao")
  val optList:Option[List[Int]] = Some(List(1,2,3,4,5))
  val optNone:Option[Int]=None
  val valuesSeq: Seq[Double] = Seq(1.0, 2.0, 3.0, 4.0, 5.0)

  //def Map[B](f: A => B): Option[B] = ???
  @Test
  def testMap =
    assertEquals(Some(2), optInt.map(_+1))
    assertEquals(Some(List(2,3,4,5,6)), optList.map(_.map(_+1))) ///Un po' strano, da verificare bene
    assertEquals(None, optNone.map(_+1))

  @Test
  def testGetOrElse =
    assertEquals(1, optInt.getOrElse(0))
    assertEquals(List(1, 2, 3, 4, 5), optList.getOrElse(List(0))) ///Un po' strano, da verificare bene
    assertEquals(0, optNone.getOrElse(0))

  @Test
  def testFlatMap =
    assertEquals(Some(2), optInt.flatMap(x => Some(x+1)))
    assertEquals(Some("Value: 1"), optInt.flatMap(x => Some(s"Value: $x")))

  @Test
  def testOrElse =
    assertEquals(Some(1), optInt.orElse(Some(2)))
    assertEquals(Some("Ciao"), optString.orElse(Some("Bau")))

  @Test
  def testFilter =
    assertEquals(Some(1), optInt.filter(_==1))
    assertEquals(Some(1), optInt.filter(_>0))

  @Test
  def testVariance =
    assertEquals(Some(2.0), variance(valuesSeq))

  @Test
  def testMap2 =
    assertEquals(Some(3), map2(optInt,optInt2)((x,y)=>x+y))
    assertEquals(None, map2(optInt,None)((x:Int,y:Int)=>x+y))
    assertEquals(None, map2(None,optInt)((x:Int,y:Int)=>x+y))
  @Test
  def testMap2v2 =
    assertEquals(Some(3), map2v2(optInt, optInt2)((x, y) => x + y))
    assertEquals(None, map2v2(optInt, None)((x: Int, y: Int) => x + y))
    assertEquals(None, map2v2(None, optInt)((x: Int, y: Int) => x + y))

  /*
  @Test
  def testSequence =
    assertEquals(Some(List(1,2,3,4,5)), sequence(List(Some(1),Some(2),Some(3),Some(4),Some(5))))
    assertEquals(None, sequence(List(Some(1),None,Some(3),Some(4),Some(5))))

  */