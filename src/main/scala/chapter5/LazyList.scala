package chapter5

import scala.annotation.tailrec

import LazyList.*
enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: ()=> LazyList[A])
  //Ex 5.1
  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  def toListTr: List[A] =
    @annotation.tailrec
    def go(ll:LazyList[A], acc: List[A]):List[A] =  ll match
      case Cons(h, t) => go(t(), h()::acc)
      case Empty => acc.reverse
    go(this, Nil)

  //Ex5.2
  //Stack safe because of pattern matching and lazy evaluation but NONT tailrec
  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty) //Special case, more efficient
    case _ => empty

  @annotation.tailrec
  final def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this

  //Ex 5.3
  def takeWhile(pred: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if pred(h()) => cons(h(), t().takeWhile(pred))
    case _ => empty

  //Par5.3 examples of lazy functions that can EARLY TERMINATE.
  def exists(p: A => Boolean): Boolean = this match
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match
    case Cons(h, t) => f(h(), t().foldRight(acc)(f))
    case _ => acc

  def existsFr(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  //Ex 5.4
  def forAllFr(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  @tailrec
  final def forAll(p: A => Boolean): Boolean = this match
    case Cons(h, t) => p(h()) && t().forAll(p)
    case Empty => true

  //Ex5.5
  def takeWhileFr(pred: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if pred(a) then cons(a, b) else empty)

  //Ex 5.6
  /*def headOption[A](ll: LazyList[A]): Option[A] = ll match
    case Empty => None
    case Cons(h,_) => Some(h())*/
  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  //Ex 5.7: map, filter, flatMap with foldRight
  def map[B](f: A => B): LazyList[B] =
    foldRight(empty)((a, b) => cons(f(a), b))

  def filter(pred: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if pred(a) then cons(a, b) else b)

  def append[A2>:A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that)((a, b) => cons(a, b))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

object LazyList:
  def cons[A](hd: =>A, tl: => LazyList[A]):LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  //Ex 5.8
  def continually1[A](a: A): LazyList[A] =
    cons(a, continually(a))
  def continually[A](a: A): LazyList[A] =
    lazy val single: LazyList[A] = cons(a, single)
    single

  //Ex 5.9
  def from(n: Int): LazyList[Int] =
    cons(n, from(n+1))

  //Ex 5.10
  def fibs: LazyList[Int] =
    def goFibs(current: Int, next: Int): LazyList[Int] =
      cons(current, goFibs(next, current + next))
    goFibs(0, 1)

  //Ex 5.11
  def unfold[A, S](state: S)(f: S => Option[(A,S)]): LazyList[A] = f(state) match
    case Some(a, s) => cons(a, unfold(s)(f))
    case None => empty

  //Ex 5.12
  def fibsUnf:LazyList[Int] =
    unfold((0,1)):
      case(current,next) =>
        Some((current, (next, current + next)))

  def fromUnf(n: Int): LazyList[Int] =
    unfold(n)(n=>Some(n, n+1))

  def continuallyUnf[A](a: A): LazyList[A] =
    unfold(a)(n=>Some(n, n))

  //Ex 5.13
  def onesUnf =
    unfold(1)(_=>Some(1,1))

  def mapUnf[A,B](ll:LazyList[A])(f: A=>B): LazyList[B] =
    unfold(ll):
      case Cons(h, t) => Some(f(h()), t())
      case _ => None

  //Notazione abbreviata (Scala 3) di:
  /*
  def mapUnf[A, B](ll: LazyList[A])(f: A => B): LazyList[B] =
    unfold(ll) { s =>
      s match
        case Cons(h, t) => Some(f(h()), t())
        case _ => None
    }
  */
  def takeUnf[A](ll:LazyList[A], n:Int): LazyList[A] =
    unfold(ll, n):
      case (Cons(h,_),1) => Some(h(), (empty, 0))
      case (Cons(h,t),n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None

  def takeWhileUnf[A](ll:LazyList[A], f: A=>Boolean): LazyList[A] =
    unfold(ll):
      case Cons(h,t) if f(h()) => Some(h(),t())
      case _ => None

  def zipWithUnf[A, B, C](l1: LazyList[A], l2: LazyList[B], f: (A, B) => C): LazyList[C] =
    unfold(l1, l2):
      case (Cons(x,xs), Cons(y, ys)) => Some(f(x(),y()), (xs(), ys()))
      case (Empty, _) => None
      case (_, Empty) => None

  def zipAllUnf[A,B](l1:LazyList[A], l2:LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold(l1 , l2):
      case (Empty, Empty) => None
      case (Cons(h1, t1), Empty) => Some((Some(h1()) , None) , (t1() ,Empty))
      case (Empty, Cons(h2, t2)) => Some((None , Some(h2())) , (Empty , t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()) , Some(h2())) , (t1() , t2()))
/*
  def startsWith[A](list: LazyList[A], prefix: LazyList[A]): Boolean =
    unfold(list, prefix):
      case (Empty, Empty) => false
*/

