package chapter3

  enum Tree[+A]:
    case Leaf(value: A)
    case Branch(left: Tree[A], right: Tree[A])
    def size: Int = this match
      case Leaf(_) => 1
      case Branch(l, r) =>  l.size + r.size +1
    //Ex 3.27
    def map[B](f: A=>B): Tree[B] = this match
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(l.map(f), r.map(f))
    //Ex 3.26
    def depth: Int = this match
      case Leaf(_) => 0
      case Branch(l, r) => 1 + l.depth.max(r.depth)

    //Ex 3.28
    def fold[B](f: A => B, g:(B,B) => B):B = this match
      case Leaf(a) => f(a)
      case Branch(l, r) => g(l.fold(f,g), r.fold(f,g))

    def sizeF:Int =
      fold(_=>1,_+_+1)
    def depthF:Int =
      fold(_=>0, 1+_.max(_))
    def mapF[B](f: A=>B):Tree[B] =
      fold(a=>Leaf(f(a)), Branch(_,_))

  object Tree:
    /*
    def size[A](t: Tree[A]): Int = t match
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
     */

    /*
    //size with extension method
    extension [A](t:Tree[A]) def size: Int = t match
      case Leaf(_) => 1
      case Branch(l, r) => 1 + l.size + r.size
    */

    //Ex 3.25
    //With extension method so I can call maximum as a method of Tree
    extension (t:Tree[Int]) def maximum: Int = t match
      case Leaf(n) => n
      case Branch(l,r) => l.maximum.max(r.maximum)

    //Ex 3.28
    extension (t: Tree[Int]) def maximumF: Int =
      t.fold(a=>a, _ max _ )
    /*
    //without extension method
    def maximum(t: Tree[Int]): Int = t match
      case Leaf(n) => n
      case Branch(l, r) => maximum(l).max(maximum(r))
    */
    //Ex 3.26
    /*def depth[A](t: Tree[A]): Int =
      def goDepth(t: Tree[A], acc:Int): Int = t match
        case Leaf(_) => acc
        case Branch(l,r) => goDepth(l,acc+1).max(goDepth(r,acc+1))
      goDepth(t, 0)
    */
    /*
    def depth[A](t: Tree[A]):Int = t match
      case Leaf(_) => 0
      case Branch(l, r) => 1+depth(l).max(depth(r))

     */
    //Ex 3.27
    /*
    def map[A,B](t: Tree[A], f:A=>B): Tree[B] = t match
      case Leaf(x) => Leaf(f(x))
      case Branch(l,r) => Branch(map(l,f), map(r,f))

     */
