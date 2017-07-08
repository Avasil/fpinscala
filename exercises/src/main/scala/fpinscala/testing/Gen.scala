package fpinscala.testing

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.laziness.Stream
import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par
import fpinscala.state._
import fpinscala.testing.Gen._
import fpinscala.testing.Prop._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase,
                     successes: SuccessCount) extends Result {
  def isFalsified: Boolean = true
}

case object Proved extends Result {
  def isFalsified: Boolean = false
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => {
      this.run(max, n, rng) match {
        case Passed | Proved => p.run(max, n, rng) match {
          case Passed | Proved => Passed
          case Falsified(failure, successes) =>
            Falsified(failure, successes + n)
        }
        case Falsified(failureL, successesL) => p.run(max, n, rng) match {
          case Falsified(failureR, successesR) =>
            Falsified(failureL + " && " + failureR, successesL + successesR)
          case Passed | Proved =>
            Falsified(failureL, successesL + n)
        }
      }
    }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => {
      this.run(max, n, rng) match {
        case Passed | Proved => Passed
        case Falsified(failureL, successesL) => p.run(max, n, rng) match {
          case Falsified(failureR, successesR) =>
            Falsified(failureL + " && " + failureR, successesL + successesR)
          case Passed | Proved => Passed
        }
      }
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int
  type TestCases = Int

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def check(p: => Boolean): Prop = { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  val S: Gen[ExecutorService] = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

}

case class Gen[A](sample: State[RNG, A]) {

  def unsized: SGen[A] =
    SGen(_ => this)

  def map[B](f: A => B): Gen[B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(n: Gen[Int]): Gen[List[A]] = {
    n.flatMap(listOfN)
  }

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g) ((_, _))
}

object Gen {

  def unit[A](a: => A): Gen[A] =
    Gen(State(RNG.unit(a)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(bool => if (bool) g1 else g2)
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (gen1, weight1) = g1
    val (gen2, weight2) = g2

    val chance1 = weight1.abs / (weight1.abs + weight2.abs)

    Gen(State(RNG.double).flatMap(res => if (res < chance1) gen1.sample else gen2.sample))
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(g.listOfN)

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))

}

case class SGen[A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] =
    forSize(n)

  def map[B](f: A => B): SGen[B] =
    SGen(forSize(_) map f)

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    SGen(n => forSize(n) flatMap (a => f(a).forSize(n)))
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] =
    SGen(n => apply(n) ** s2(n))

}

object Testing {
  def testMax = {
    val smallInt = Gen.choose(-10, 10)
    val maxProp: Prop = forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    run(maxProp)
  }

  def testSorted = {
    val smallInt = Gen.choose(-10, 10)
    val sortedProp: Prop = forAll(listOf(smallInt)) { ns =>
      val sorted = ns.sorted
      sorted.isEmpty || sorted.tail.isEmpty || sorted.zip(sorted.tail).forall { case (a, b) => b >= a }
    }

    run(sortedProp)
  }

  def testPar = {
    val pint: Gen[Par[SuccessCount]] = Gen.choose(0, 10) map Par.unit
    val p4 =
      forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

    run(p4)

    val pint2 =
      Gen
        .choose(0, 10)
        .listOfN(10)
        .map(l => Par.unit(l.map(Par.unit)))

    // fork(x) == x
    val forkProp = forAllPar(pint2)(n => equal(Par.fork(n), n))

    run(forkProp)

  }
}
