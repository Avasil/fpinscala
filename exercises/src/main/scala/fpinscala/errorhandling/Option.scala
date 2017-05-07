package fpinscala.errorhandling


import scala.{Either => _, Option => _, Some => _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap { case x if f(x) => Some(x) }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // mean(math.pow(x - m, 2)) for each element
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m =>
      mean(xs.map(x => math.pow(x - m, 2))))


  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(sA => b.map(sB => f(sA, sB)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(List()): Option[List[A]])((elem, acc) => map2(elem, acc)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(List()))((elem, acc) => map2(f(elem), acc)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)
}