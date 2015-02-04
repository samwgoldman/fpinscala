package fpinscala.laziness

import Stream._
trait Stream[+A] {
  def toList: List[A] =
    foldRight(List[A]())(_ :: _)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] =
    unfold((n, this)) {
      case (1, Cons(h, t)) => Some(h(), (0, empty))
      case (n, Cons(h, t)) => Some(h(), (n - 1, t()))
      case _ => None
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, xs) if (n > 0) => xs().take(n - 1)
      case _ => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(a, as), Cons(b, bs)) => Some(f(a(), b()), (as(), bs()))
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s)) {
      case (Empty, Empty) => None
      case (Empty, Cons(b, bs)) => Some((None, Some(b())), (Empty, bs()))
      case (Cons(a, as), Empty) => Some((Some(a()), None), (as(), Empty))
      case (Cons(a, as), Cons(b, bs)) => Some((Some(a()), Some(b())), (as(), bs()))
    }

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b.append(s)))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.nonEmpty).forAll { case (a, b) => a == b }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = unfold(1)(x => Some(x, x))

  def constant[A](a: A): Stream[A] =
    unfold(a)(x => Some(x, x))

  def from(n: Int): Stream[Int] =
    unfold(n)(x => Some(x, x + 1))

  def fibs: Stream[Int] = {
    unfold((0, 1)) { case (a, b) => Some(a, (b, a + b)) }
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => Empty
    }
}
