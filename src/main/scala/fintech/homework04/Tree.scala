package fintech.homework04

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // реализовать функцию fold
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    t match {
      case Leaf(value) => f(value)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }
  }

  // реализовать следующие функции через fold

  def size[A](t: Tree[A]): Int = fold[A, Int](t)(_ => 1)((l, r) => l + r)

  def max(t: Tree[Int]): Int = fold[Int, Int](t)(a => a)((l, r) => math.max(l, r))

  def depth[A](t: Tree[A]): Int = fold[A, Int](t)(_ => 0)((l, r) => math.max(l, r) + 1)


  //здесь возможно придется прибегнуть к насильному указанию типа: Leaf(a): Tree[A]
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(a => Leaf(f(a)))((l, r) => new Branch[B](l, r))
}
