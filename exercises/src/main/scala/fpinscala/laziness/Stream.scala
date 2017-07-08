package fpinscala.laziness

import fpinscala.laziness.Stream._

import scala.annotation.tailrec
import scala.{Stream => _}

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }


  def toList: List[A] = {

    @tailrec
    def loop(stream: Stream[A], acc: List[A]): List[A] = stream match {
      case Empty => acc
      case Cons(h, t) => loop(t(), h() :: acc)
    }

    loop(this, List()).reverse
  }

  //  def take(n: Int): Stream[A] = {
  //
  //    @tailrec
  //    def loop(stream: Stream[A], result: Stream[A], acc: Int): Stream[A] = stream match {
  //      case _ if acc == 0 => result
  //      case Empty => result
  //      case Cons(h, t) => loop(t(), cons(h(), result), acc - 1)
  //    }
  //
  //    loop(this, Empty, n)
  //  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // foldleft accumulates values and foldRight just get tail
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  // if h is empty then foldRight returns "zero" right away
  def headOption: Option[A] =
    foldRight[Option[A]](None)(
      (h, _) => Some(h)
    )

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])(
      (a, b) => if (p(a)) cons(a, b) else b
    )

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  // map, take, takeWhile, zipWith, zipAll via unfold

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case ((Cons(h, t), acc)) if acc > 0 => Some(h(), (t(), acc - 1))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _ => None
    }

  def zipWithViaUnfold[B >: A](b: Stream[B])(f: (B, B) => B): Stream[B] =
    unfold((this, b)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAllViaUnfold[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())) -> (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None) -> (t1(), empty[B]))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())) -> (empty[A], t2()))
      case _ => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // special case of `zipWith`
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  def startsWith[B](s: Stream[B]): Boolean =
    zipAllViaUnfold(s).takeWhile {
      case (Some(s1), Some(s2)) => s1 == s2
      case (_, None) => true
      case _ => false
    } match {
      case Cons(_, _) => true
      case _ => false
    }

  def startsWithBetter[B](s: Stream[B]): Boolean =
    zipAllViaUnfold(s).takeWhile(_._2.isDefined) forAll {
      case (h, h2) => h == h2
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Cons(h, t) => Some(cons(h(), t()), t())
      case _ => None
    } append Stream(empty)

  def hasSubsequence[B](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream(z)) { case (a, b@Cons(h, _)) => cons(f(a, h()), b) }
  
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

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = cons(a, tail)

    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs(n: Int = 0, m: Int = 1): Stream[Int] =
    cons(n, fibs(m, n + m))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z)
      .map { case (a, s) => cons(a, unfold(s)(f)) }
      .getOrElse(empty[A])

  //  fibs from constant

  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (x, y) => Some(x, (y, x + y)) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(x => Some(x, x + 1))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  def onesViaUnfold: Stream[Int] =
    constantViaUnfold(1)
}