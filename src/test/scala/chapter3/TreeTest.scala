package chapter3

import chapter3.Tree.*
import org.junit.*
import org.junit.Assert.*

class TreeTest:
  val t1:Tree[Int] = Branch(Leaf(1),Leaf(2))
  val t2:Tree[Int] = Branch(Branch(Leaf(1),Leaf(8)),Branch(Leaf(3),Leaf(4)))
  val t3:Tree[Int] = Branch(
                        Branch(Leaf(1),Leaf(2)),
                        Branch(
                          Branch(
                            Branch(Leaf(3),Leaf(4)),
                            Branch(Leaf(5),Leaf(6))),
                          Branch(Leaf(7),Leaf(8))))
  val t4:Tree[Double] = Branch(Leaf(1.0),Leaf(2.0))
  @Test
  def testSize =
    assertEquals(3,t1.size)
    assertEquals(7,t2.size)
    //assertEquals(3, t1.size) //Test for extension method version
    //assertEquals(7, t2.size) //Test for extension method version
  @Test
  def testMaximum =
    assertEquals(2, t1.maximum)
    assertEquals(8, t2.maximum)
    assertEquals(8, t3.maximum)

  @Test
  def testDepth =
    //assertEquals(1, depth(t1))
    //assertEquals(2, depth(t2))
    //assertEquals(4, depth(t3))
    assertEquals(1,t1.depth) //Test for depth as a method defined in enum Tree[+A]
    assertEquals(2,t2.depth) //Test for depth as a method defined in enum Tree[+A]
    assertEquals(4,t3.depth) //Test for depth as a method defined in enum Tree[+A]


  @Test
  def testMap =
    assertEquals(Branch(Leaf(2),Leaf(3)), t1.map(_+1))
    assertEquals(Branch(Branch(Leaf(2),Leaf(9)),Branch(Leaf(4),Leaf(5))), t2.map(_+1))
    assertEquals(Branch(
      Branch(Leaf(2), Leaf(3)),
      Branch(
        Branch(
          Branch(Leaf(4), Leaf(5)),
          Branch(Leaf(6), Leaf(7))),
        Branch(Leaf(8), Leaf(9)))), t3.map(_+1))

  @Test
  def testSizeF =
    assertEquals(3, t1.sizeF)
    assertEquals(7, t2.sizeF)
    //assertEquals(3, t1.size) //Test for extension method version
    //assertEquals(7, t2.size) //Test for extension method version

  @Test
  def testMapF =
    assertEquals(Branch(Leaf(2), Leaf(3)), t1.mapF(_ + 1))
    assertEquals(Branch(Branch(Leaf(2), Leaf(9)), Branch(Leaf(4), Leaf(5))), t2.mapF(_ + 1))
    assertEquals(Branch(
      Branch(Leaf(2), Leaf(3)),
      Branch(
        Branch(
          Branch(Leaf(4), Leaf(5)),
          Branch(Leaf(6), Leaf(7))),
        Branch(Leaf(8), Leaf(9)))), t3.mapF(_ + 1))
  @Test
  def testDepthF =
    //assertEquals(1, depth(t1))
    //assertEquals(2, depth(t2))
    //assertEquals(4, depth(t3))
    assertEquals(1, t1.depthF) //Test for depth as a method defined in enum Tree[+A]
    assertEquals(2, t2.depthF) //Test for depth as a method defined in enum Tree[+A]
    assertEquals(4, t3.depthF) //Test for depth as a method defined in enum Tree[+A]
  @Test
  def testMaximumF =
    assertEquals(2, t1.maximumF)
    assertEquals(8, t2.maximumF)
    assertEquals(8, t3.maximumF)