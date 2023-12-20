package chapter5


import scala.annotation.tailrec
import scala.collection.immutable.LazyList.cons

enum LazyListF[+A]:
  case Empty
  case Cons(h: () => A, t: ()=> LazyListF[A])

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList

object LazyListF:
  def cons[A](hd: =>A, tl: => LazyListF[A]):LazyListF[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyListF[A] = Empty

  def apply[A](as: A*): LazyListF[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  //Ex 5.1
  def toList[A](ll:LazyListF[A]):List[A] = ll match
    case Empty => Nil
    case Cons(h,t) =>  h()::toList(t())
  //Ex5.2
  //Stack safe because of pattern matching and lazy evaluation but NONT tailrec
  def take[A](ll:LazyListF[A], n:Int):LazyListF[A] = ll match
      case Cons(h,t) if n>1 => cons(h(), take(t(), n-1))
      case Cons(h,_) if n==1 => cons(h(), empty) //Special case, more efficient
      case _ => empty

  @annotation.tailrec
  def drop[A](ll:LazyListF[A], n:Int):LazyListF[A] = ll match
    case Cons(_,t) if n>0 => drop(t(), n-1)
    case _ => ll
  //Ex 5.3
  def takeWhile[A](ll:LazyListF[A], pred:A => Boolean):LazyListF[A] = ll match
    case Cons(h,t) if pred(h()) => cons(h(), takeWhile(t(), pred))
    case _ => empty
  //Par5.3 examples of lazy functions that can EARLY TERMINATE.
  def exists[A] (ll:LazyListF[A], p: A=>Boolean): Boolean = ll match
    case Cons(h,t) => p(h()) || exists(t(),p)
    case _ => false

  def foldRight[A,B](ll:LazyListF[A], acc: =>B)(f: (A, => B) =>B):B = ll match
      case Cons(h, t) => f(h(), foldRight(t(),acc)(f))
      case _ => acc
  def existsFr[A](ll:LazyListF[A], p: A => Boolean): Boolean =
    foldRight(ll,false)((a,b) => p(a) || b)

  //Ex 5.4
  def forAllFr[A](ll:LazyListF[A], p: A => Boolean): Boolean =
    foldRight(ll, true)((a,b)=>p(a) && b)
  @tailrec
  def forAll[A](ll:LazyListF[A], p: A=> Boolean) : Boolean = ll match
    case Cons(h,t) => p(h()) && forAll(t(), p)
    case Empty => true

  //Ex5.5
  def takeWhileFr[A](ll: LazyListF[A], pred: A => Boolean): LazyListF[A] =
    foldRight(ll, empty)((a,b) => if pred(a) then cons(a,b) else empty)

  //Ex 5.6
  /*def headOption[A](ll: LazyList[A]): Option[A] = ll match
    case Empty => None
    case Cons(h,_) => Some(h())*/
  def headOption[A](ll: LazyListF[A]): Option[A] =
    foldRight(ll, None: Option[A]) ((a,_) => Some(a))

  //Ex 5.7: map, filter, flatMap with foldRight
  def map[A,B](ll:LazyListF[A])(f: A => B):LazyListF[B] =
    foldRight(ll, empty) ((a,b) => cons(f(a),b) )

  def filter[A](ll:LazyListF[A])(pred: A=>Boolean):LazyListF[A] =
    foldRight(ll, empty) ((a,b) => if pred(a) then cons(a,b) else b)

  def append[A](ll1:LazyListF[A], ll2: => LazyListF[A]):LazyListF[A] =
    foldRight(ll1, ll2) ((a,b) => cons(a,b))

  def flatMap[A,B](ll:LazyListF[A])(f: A=>LazyListF[B]): LazyListF[B] =
    foldRight(ll, empty[B])( (a,b) => append(f(a),b))



