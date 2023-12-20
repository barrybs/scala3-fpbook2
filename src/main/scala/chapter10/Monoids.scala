package chapter10

object Monoids:
  trait Monoid[A]:
    def combine(a1: A, a2:A):A
    def empty: A



  val stringMonoid: Monoid[String] = new:
    def combine(a1: String, a2: String) = a1+a2
    val empty = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty = Nil

  val intAddition: Monoid[Int] = new:
    def combine(a1: Int, a2: Int) = a1+a2
    val empty=0

  val intMultiplication: Monoid[Int] = new:
    def combine(a1: Int, a2: Int) = a1 * a2
    val empty = 1

  val booleanOr: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean) = a1 || a2
    def empty = false

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean]:  //Explicit syntax
    def combine(a1: Boolean, a2: Boolean) = a1 && a2
    def empty = true

/*  def optionMonoid[A](f: (A,A) => A): Monoid[Option[A]] = new:
    def combine(x:Option[A], y:Option[A]) = x.map2(y)(f)
    val empty = None
*/
  def optionMonoid[A](f: (A, A) => A): Monoid[Option[A]] = new Monoid[Option[A]]:
    def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match
      case (Some(a), Some(b)) => Some(f(a, b))
      case (Some(a), None) => Some(a)
      case (None, Some(b)) => Some(b)
      case (None, None) => None
    val empty: Option[A] = None

  def endoMonoid[A]: Monoid[A => A] = new:
    //def combine(x: A => A, y: A => A):A => A = c => y(x(c))
    def combine(x: A => A, y: A => A):A => A = x andThen y
    def empty: A=>A = identity


