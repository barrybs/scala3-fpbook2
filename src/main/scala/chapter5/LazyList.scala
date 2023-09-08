package chapter5

import chapter5.LazyList.{Cons, Empty}

import scala.List.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: ()=> LazyList[A])

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList

object LazyList:
  def cons[A](hd: =>A, tl: => LazyList[A]):LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  def toList[A](ll:LazyList[A]):List[A] = ll match
    case Empty => Nil
    case Cons(h,t) =>  h()::toList(t())
  //Stack safe because of pattern matching and lazy evaluation but NONT tailrec
  def take[A](ll:LazyList[A], n:Int):LazyList[A] = ll match
      case Cons(h,t) if n>1 => cons(h(), take(t(), n-1))
      case Cons(h,_) if n==1 => cons(h(), empty) //Special case, more efficient
      case _ => empty

  @annotation.tailrec
  def drop[A](ll:LazyList[A], n:Int):LazyList[A] = ll match
    case Cons(_,t) if n>0 => drop(t(), n-1)
    case _ => ll

  def takeWhile[A](ll:LazyList[A], pred:A => Boolean):LazyList[A] = ll match
    case Cons(h,t) if pred(h()) => cons(h(), takeWhile(t(), pred))
    case _ => empty
