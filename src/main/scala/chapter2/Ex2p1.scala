package chapter2

object Ex2p1:
  def fib(n:Int) : Int=
    if n<=1 then n
    else
      fib(n-1)+fib(n-2)

  def fibTail(n:Int) : Int=
    @annotation.tailrec
    def g(n:Int, current:Int, next:Int):Int=
      if(n<=0) current
      else g(n-1, next, current + next)
    g(n,0,1)

  @main def printFib : Unit =
    println(fib(6))
    println(fibTail(6))
