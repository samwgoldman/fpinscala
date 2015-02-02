package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(x) => f(x)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def size[A](t: Tree[A]): Int = {
    val one = Function.const(1) _
    fold(t)(one)(_ + _)
  }

  def max(t: Tree[Int]): Int =
    fold(t)(identity)(_ max _)

  def depth[A](t: Tree[A]): Int = {
    val one = Function.const(1) _
    fold(t)(one)(1 + _.max(_))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    val leaf: A => Tree[B] = x => Leaf(f(x))
    fold(t)(leaf)(Branch(_, _))
  }
}
