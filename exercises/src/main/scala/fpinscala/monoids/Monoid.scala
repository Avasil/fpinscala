package fpinscala.monoids

import fpinscala.datastructures.Nil
import fpinscala.monoids.Monoid.{dual, endoMonoid}
import fpinscala.parallelism.Nonblocking.Par.toParOps

import scala.language.{higherKinds, postfixOps}

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    val zero = List()
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2

    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2

    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2

    def zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2

    def zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2

    def zero = None
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero = m.zero
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a1 compose a2

    def zero = (x: A) => x
  }

  import fpinscala.testing._
  import Prop._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(Gen.listOfN(3, gen)) { list =>
      list.forall(a => m.op(a, m.zero) == m.op(m.zero, a) == a) &&
        m.op(list.head, m.op(list(1), list(2))) == m.op(m.op(list.head, list(1)), list(2))
    }

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // fold + function on every element (map)
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldRight(m.zero)((a, b) => m.op(f(a), b))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    val curried: (A) => (B) => B = f.curried

    val monoid = endoMonoid[B]

    //    foldMap(as, monoid)(a => monoid.op(curried(a), monoid.zero))(z)
    foldMap(as, monoid)(curried)(z)
  }


  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    val curried: (B) => (A) => B = f.curried

    val monoid = endoMonoid[B]

    foldMap(as, monoid)(a => (b: B) => curried(b)(a))(z)
  }

  //  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
  //    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = as match {

    case x if x.isEmpty =>
      m.zero

    case x if x.length == 1 =>
      f(as(0))

    case _ =>
      val (as1, as2) = as.splitAt(as.length / 2)
      m.op(foldMapV(as1, m)(f), foldMapV(as2, m)(f))
  }


  def ordered(ints: IndexedSeq[Int]): Boolean = {
    def orderedIntervalMonoid: Monoid[Option[(Int, Int, Boolean)]] = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(x: Option[(Int, Int, Boolean)], y: Option[(Int, Int, Boolean)]) =
        (x, y) match {
          case (Some((xStart, xEnd, xOrdered)), Some((yStart, yEnd, yOrdered))) =>
            Some(xStart min yStart, xEnd max yEnd, xOrdered & yOrdered & (xStart <= yEnd))

          case (None, a) => a
          case (a, None) => a
        }

      def zero = None
    }

    foldMapV(ints, orderedIntervalMonoid)(elem => Some(elem, elem, true)).forall(_._3)
  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  import fpinscala.parallelism.Nonblocking._

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = for {
      x <- a1
      y <- a2
    } yield m.op(x, y)

    def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f)
      .flatMap(foldMapV(_, par(m))(Par.unit))

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC) = (a1, a2) match {
      case (Stub(l), Stub(r)) => Stub(l + r)
      case (Stub(l), Part(lStub, words, rStub)) => Part(l + lStub, words, rStub)
      case (Part(lStub, words, rStub), Stub(r)) => Part(lStub, words, rStub + r)
      case (Part(lStub, words, rStub), Part(lrStub, rwords, rrStub)) =>
        val joined = if ((rStub + lrStub) nonEmpty) 1 else 0

        Part(lStub, words + rwords + joined, rrStub)
    }

    def zero = Stub("")
  }

  def count(s: String): Int = {
    def wc(c: Char) =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(string) =>
        if (string.nonEmpty) 1 else 0
      case Part(l, length, r) =>
        (if (l.nonEmpty) 1 else 0) + length + (if (r.nonEmpty) 1 else 0)
    }
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)) =
        (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

      override def zero = (A.zero, B.zero)
    }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      override def op(a1: (A) => B, a2: (A) => B) =
        (a: A) => B.op(a1(a), a2(a))

      override def zero =
        _ => B.zero
    }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()

      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((elem: A) => Map(elem -> 1))
}

trait Foldable[F[_]] {

  def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {

  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMap(as, mb)(f)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {

  import Monoid._

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {

  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(value) => f(value)
      case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
    }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    as match {
      case Leaf(value) =>
        f(z, value)
      case Branch(left, right) =>
        foldLeft(right)(foldLeft(left)(z)(f))(f)
    }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    as match {
      case Leaf(value) => f(value, z)
      case Branch(left, right) =>
        foldRight(left)(foldRight(right)(z)(f))(f)
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as map f getOrElse mb.zero

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as map f.curried(z) getOrElse z

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case None => z
    case Some(a) => f(a, z)
  }
}

