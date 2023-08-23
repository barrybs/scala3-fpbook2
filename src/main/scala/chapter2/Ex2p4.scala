package chapter2

object Ex2p4:
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)
  def uncurry[A,B,C](f: A=>(B=>C)): (A,B) => C =
    (a:A,b:B) => f(a)(b)

