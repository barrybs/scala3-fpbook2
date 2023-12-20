package chapter10
import chapter10.Monoids.*
import org.junit.Assert.assertEquals
import org.junit.Test
class MonoidsTest:
  val s1 = "Hello "
  val s2 = "World"
  val i1 = 2
  val i2 = 3
  val b1 = true
  val b2 = false
  val so1 = Some("Hello ")
  val so2 = Some("World")
  val so3 = None

  @Test
  def testStringMonoid =
    assertEquals("Hello World", stringMonoid.combine(s1, s2))
  @Test
  def testIntAdditionMonoid =
    assertEquals(5, intAddition.combine(i1, i2))
  @Test
  def testIntMultiplicationMonoid =
    assertEquals(6, intMultiplication.combine(i1, i2))

  @Test
  def testBooleanOrMonoid =
    assertEquals(true, booleanOr.combine(b1, b2))

  @Test
  def testBooleanAndMonoid =
    assertEquals(false, booleanAnd.combine(b1, b2))

  @Test
  def testOptionMonoid =
    assertEquals(Some("Hello World"), optionMonoid[String](_++_).combine(so1, so2))

  @Test
  def testEndoMonoid =
    val f: Int => Int = _ + 1
    val g: Int => Int = _ * 2
    val monoid = endoMonoid[Int]
    val combinedFunction = monoid.combine(f, g)
    assertEquals(6,combinedFunction(2))