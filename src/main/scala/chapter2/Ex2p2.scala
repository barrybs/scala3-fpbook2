package chapter2

import scala.annotation.tailrec

object Ex2p2:
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
    @tailrec
    def check(i: Int): Boolean =
      if i+1 >= as.length then true
      else
        if gt(as(i), as(i + 1)) then false
        else check(i + 1)
    check(0)
  @main
  def testIsSorted123gt =
    assert(isSorted(Array(1, 2, 3), _ > _))
  @main
  def testIsSorted121gt =
    assert(!isSorted(Array(1, 2, 1), _ > _))
  @main
  def testIsSorted321lt =
    assert(isSorted(Array(3, 2, 1), _ < _))
  @main
  def testIsSorted123lt =
    assert(!isSorted(Array(1, 2, 3), _ < _))
  @main
  def testIsSorted0lt =
    assert(isSorted(Array[Int](), _ < _))
  @main
  def testIsSorted1lt =
    assert(isSorted(Array(1), _ < _))