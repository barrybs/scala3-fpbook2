package chapter3

import scala.annotation.tailrec
  enum List[+A]:
    case Nil
    case Cons(head: A, tail: List[A])
  object List:
    def apply[A] (as: A*): List[A] =
      if as.isEmpty then Nil
      else Cons(as.head, apply(as.tail*))
    def sum(ints: List[Int]): Int = ints match
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)

    //Ex 3.2
    /*def tail[Int](l:List[Int]): List[Int] = l match
      case Nil => sys.error("Your list is Nil!")
      case Cons(_,Cons(h,t)) => Cons(h,t)
    */
    def head[A](l:List[A]):List[A] = l match
      case Nil => throw new NoSuchElementException("head of empty list")
      case Cons(h,_) => Cons(h, Nil)

    def tail[A](l:List[A]):List[A] = l match
      case Nil => sys.error("Your list is Nil!")
      case Cons(_,t) => t
    //Ex 3.3
    def insert[A](h:A, l:List[A]):List[A] = l match
      case Nil => Cons(h, Nil)
      case Cons(hh,t) => Cons(h,Cons(hh,t))
    //Ex 3.4
    @tailrec
    def drop[A](as:List[A], n:Int):List[A] =
      if n <=0 then as
      else as match
        case Cons(_,t) => drop(t, n-1)
        case Nil => Nil

    //Ex 3.5
    /*
    //Without pattern guard version
    def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match
      case Cons(h, t) => if f(h) then dropWhile(t, f) else as
      case Nil => as
    */
    //Pattern guard version
    @tailrec
    def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => as
    //ex 3.6
    def init[A](as: List[A]) : List[A] = as match
      case Cons(_, Nil) | Nil => Nil
      case Cons(h,t) => Cons(h,init(t))
    def foldRight[A,B](as: List[A], acc:B, f: (A, B) => B) : B =
      as match
        case Nil => acc
        case Cons(x, xs) => f(x, foldRight(xs, acc, f))
    //Ex 3.9
    def length[A](as: List[A]): Int =
      foldRight(as, 0, (_, acc) => acc + 1)
    //Ex 3.10
    @annotation.tailrec
    def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B = as match
      case Nil => acc
      case Cons(h, t) => foldLeft(t, f(acc, h), f)
    def foldRightWithFoldLeft1[A,B](as: List[A], acc: B, f: (A, B) => B): B =
      foldLeft(reverse(as), acc, (b,a) => f(a,b))
    def foldRightWithFoldLeft2[A, B](as: List[A], acc: B, f: (A, B) => B): B =
      foldLeft(as, (b:B)=>b, (g,a) => b => g(f(a, b)))(acc)

    def foldLeftWithFoldRight[A, B](as: List[A], acc:B, f: (B,A) => B): B =
      foldRight(as, (b:B)=>b, (a,g) => b =>g(f(b,a)))(acc)

    def lengthFl[A](as: List[A]): Int =
      foldLeft(as, 0, (acc,_) => acc+1)

    //Ex 3.11
    def sumFl(as: List[Int]) : Int =
      foldLeft(as,0, _+_)
    def prodFl(as: List[Int]) : Int =
      foldLeft(as,1, _*_)
    def append[A](a1:List[A], a2:List[A]): List[A] =
      a1 match
        case Nil => a2
        case Cons(h, t) => Cons(h, append(t,a2))
    //Ex3.14
    def appendFl[A](a1:List[A], a2:List[A]): List[A] =
      foldRight(a1, a2, Cons(_,_))
    //Ex 3.15
    def concat[A](l:List[List[A]]):List[A] =
      foldRight(l, Nil:List[A], append)
    //Ex 3.16
    def incrementEach(l:List[Int]):List[Int] = l match
      case Nil => Nil
      case Cons(x, xs) => Cons(x+1, incrementEach(xs))
    //Ex 3.16 with foldRight
    def incrementEachFr(l:List[Int]):List[Int] =
      foldRight(l, Nil:List[Int], (x, acc) => Cons(x+1, acc))

    //Ex 3.17
    def dToS(l : List[Double]): List[String] = l match
      case Nil => Nil
      case Cons(x, xs) => Cons(x.toString, dToS(xs))

    def dToSFr(l: List[Double]): List[String] =
      foldRight(l, Nil:List[String], (x , acc) => Cons(x.toString, acc))

    //Ex 3.19
    /*def filter[A](l:List[A], f:A=>Boolean): List[A] = l match
      case Nil => Nil
      case Cons(x, xs) if f(x) => Cons(x,filter(xs,f))
      case Cons(_, xs) => filter(xs,f)
    */
    /*def filter[A](l: List[A], f: A => Boolean): List[A] = l match
      case Nil => Nil
      case Cons(x, xs) => if f(x) then Cons(x, filter(xs, f)) else filter(xs,f)
    */
    def filter[A](as: List[A], f: A => Boolean): List[A] =
      foldRightWithFoldLeft1(as, Nil:List[A], (x, acc) => if f(x) then Cons(x, acc) else acc)
    //Ex 3.18
    def map[A, B](l: List[A], f: A => B): List[B] =
      foldRight(l, Nil:List[B], (x, acc) => Cons(f(x), acc))
    //  foldRightWithFoldLeft1(l, Nil: List[B], (x, acc) => Cons(f(x), acc))
    //Ex 3.20
    def flatMap2[A,B](as:List[A], f: A => List[B]): List[B] =
      foldRight(as, Nil:List[B], (a, acc) => append(f(a), acc))
    def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
      concat(map(as, f))
    //Ex 3.21
    def filter2[A](as: List[A], f: A => Boolean): List[A] =
      flatMap(as, a => if f(a) then List(a) else Nil)

    //Ex 3.22
    //Ok but different from the red book solution
    def addPairWise1(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match
      case (Nil, Cons(y, ys)) => Cons(y, addPairWise1(Nil, ys))
      case (Cons(x, xs), Nil) => Cons(x, addPairWise1(xs, Nil))
      case (Cons(x,xs), Cons(y,ys)) => Cons(x+y, addPairWise1(xs,ys))
      case _ => Nil
    //Red book solution
    def addPairWise2(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPairWise1(xs, ys))

    //Ex 3.23
    def combine[A,B,C](l1: List[A], l2: List[A], f: (A,A)=>A): List[A] = (l1, l2) match
      case (Cons(x,xs), Cons(y,ys)) => Cons(f(x,y), combine(xs,ys,f))
      case (Nil, Cons(y, ys)) => Cons(y, combine(Nil, ys,f))
      case (Cons(x, xs), Nil) => Cons(x, combine(xs, Nil,f))
      case _ => Nil

    def zipWith[A,B,C](l1: List[A], l2: List[B], f: (A, B) => C): List[C] = (l1, l2) match
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys, f))

    def zipWithTr[A,B,C](l1: List[A], l2: List[B], f: (A, B) => C): List[C] =
      @annotation.tailrec
      def loop (a: List[A], b:List[B], acc: List[C]): List[C] = (a, b) match
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(x, xs), Cons(y, ys)) => loop(xs,ys,Cons(f(x,y), acc))
      reverse(loop(l1,l2,Nil))



      //foldRight(l,Nil:List[A], )
    //Ex 3.12
    def reverse[A](as: List[A]) : List[A] =
      @tailrec
      def helper[A](as:List[A], acc:List[A]): List[A] = as match
        case Nil => acc
        case Cons(x,xs) => helper(xs, Cons(x,acc))
      helper(as, Nil:List[A])
    def reverseFl[A](as: List[A]) : List[A] =
      foldLeft(as, Nil:List[A], (acc, a) => Cons(a,acc))

    //Extra exercises
    def myDelete[A](as: List[A], f: A => Boolean): List[A] = as match
      case Cons(h, t) => if f(h) then myDelete(t, f) else Cons(h, myDelete(t, f))
      case Nil => Nil
    def take[A](as: List[A], n: Int): List[A] =
      if n <= 0 then Nil
      else as match
        case Cons(h, t) => Cons(h, take(t, n - 1))
        case Nil => Nil
    def takeWhile[A](as: List[A], f: A => Boolean): List[A] = as match
      case Cons(h, t) if f(h) => Cons(h, takeWhile(t, f))
      case _ => Nil
    def forAll1[A](as: List[A], pred: A=>Boolean): Boolean = as match
      case Cons(h,_) if !pred(h) => false
      case Cons(_,t) => forAll1(t,pred)
      case _ => true
    def forAll2[A](as: List[A], pred: A => Boolean): Boolean = as match
      case Cons(h, t) => if !pred(h) then false else forAll2(t, pred)
      case _ => true
    def forAllTr[A](as: List[A], pred: A => Boolean): Boolean =
      @annotation.tailrec
      def goForAll[A](as: List[A], pred: A => Boolean, acc: Boolean): Boolean = as match
        case Cons(h, t) => if !pred(h) then false else goForAll(t, pred, pred(h))
        case _ => true
      goForAll(as,pred,false)

    def exists1[A](as: List[A], pred: A => Boolean): Boolean = as match
      case Cons(h, _) if pred(h) => true
      case Cons(_, t) => exists1(t, pred)
      case _ => false

    def exists2[A](as: List[A], pred: A => Boolean): Boolean = as match
      case Cons(h, t) => if pred(h) then true else exists2(t, pred)
      case _ => false

    def exists[A](as: List[A], pred: A => Boolean): Boolean =
      @tailrec
      def goExists[A](l: List[A], pred: A => Boolean, acc:Boolean) : Boolean = l match
        case Cons(h, t) => if pred(h) then true else goExists(t, pred, pred(h))
        case _ => false
      goExists(as, pred, false)

    //Ex 3.24
    def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean =
      @tailrec
      def goHss[A](sup: List[A], sub: List[A], acc:Boolean): Boolean = (sup, sub) match
        case (Cons(h1, t1), Cons(h2, t2)) => if h1 == h2 then goHss(t1, t2,true) else goHss(t1, sub, false)
        case (Nil, Cons(_,_)) => false
        case (_, _) => acc
      goHss(sup, sub, false)

    //Book Solution
    @tailrec
    def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match
      case (_, Nil) => true
      case (Cons(h,t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
      case _ => false
    @tailrec
    def hasSubSequenceBook[A](sup: List[A], sub: List[A]): Boolean = sup match
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(h,t) => hasSubSequenceBook(t, sub)

