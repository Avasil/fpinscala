package fpinscala
package monads

import fpinscala.parallelism._
import fpinscala.parsing._
import fpinscala.state._
import fpinscala.testing._

import scala.language.higherKinds


trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = {
    traverse(lma)(identity)
  }

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight[M[List[B]]](unit(List()))((a, m) => map2(f(a), m)(_ +: _))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    map2(ma, mb)((_, _))

  // TODO: test
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = ms match {
    case Nil => unit(Nil)
    case h :: t =>
      map2(map(f(h))(x => if (x) List(h) else List()), filterM(t)(f))(_ ++ _)
  }


  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    (a: A) => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = {
    compose[Unit, A, B](_ => ma, f)()
  }

  def join[A](mma: M[M[A]]): M[A] = ???

  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = ???
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  import Nonblocking.Par

  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  def parserMonad[P[+ _]](p: Parsers[P]): Monad[P] = ???

  import errorhandling.{Option, Some}

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)

    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  import laziness.Stream

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)

    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma flatMap f
  }

  import datastructures.List

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)

    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      List.flatMap(ma)(f)
  }

  class StateMonads[S] {
    type StateS[A] = State[S, A]

    def stateMonad = new Monad[StateS] {
      def unit[A](a: => A): State[S, A] = State.unit(a)

      def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] =
        ma flatMap f

    }
  }

  // or
  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }


  val idMonad: Monad[Id] = ???

  def readerMonad[R] = ???
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = ???

  def flatMap[B](f: A => Id[B]): Id[B] = ???
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R, A] = ???

    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = ???
  }
}

