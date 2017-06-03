package fpinscala.parallelism

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import scala.language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future`
  // that just wraps a constant value. It doesn't use the `ExecutorService` at all.
  // It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // `map2` doesn't evaluate the call to `f` in a separate logical thread,
  // in accord with our design choice of having
  // `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))`
  // if we want the evaluation of `f` to occur in a separate thread.

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      TimeoutFuture(af, bf, f)
      // This implementation of `map2` does _not_ respect timeouts,
      // and eagerly waits for the returned futures. This means that even if you have passed in "forked"
      // arguments, using this map2 on them will make them wait.
      // It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures
      // `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`.
      //
      // In order to respect timeouts, we'd need a new `Future` implementation that
      // records the amount of time spent evaluating `af`, then subtracts that time from
      // the available time allocated for evaluating `bf`.
    }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
    map2(map2(a, b)((a2, b2) => (c2: C) => f(a2, b2, c2)), c)((dx, cx) => dx(cx))

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] =
    map2(map2(map2(a, b)((ax, bx) => (cx: C) => (dx: D) => f(ax, bx, cx, dx)), c)(_ (_)), d)(_ (_))

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
    map2(map2(map2(map2(a, b)((ax, bx) => (cx: C) => (dx: D) => (ex: E) => f(ax, bx, cx, dx, ex)), c)(_ (_)), d)(_ (_)), e)(_ (_))

  case class TimeoutFuture[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    val resultCache: AtomicReference[Option[C]] = new AtomicReference(None)

    def isDone = a.isDone || b.isDone

    def isCancelled = a.isCancelled || b.isCancelled

    def get = compute(Long.MaxValue)

    def get(timeout: Long, units: TimeUnit) =
      compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    def cancel(evenIfRunning: Boolean): Boolean =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    def compute(timeout: Long): C = resultCache.get() match {
      case Some(result) => result
      case None =>
        val time = System.nanoTime()

        val resultA = a.get(timeout, TimeUnit.NANOSECONDS)
        val newTime = System.nanoTime - time
        val resultB = b.get(timeout - newTime, TimeUnit.NANOSECONDS)

        val result = f(resultA, resultB)
        resultCache.set(Some(result))
        result
    }
  }

  // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one,
  // the outer `Callable` will block waiting for the "inner" task to complete.
  // Since this blocking occupies a thread in our thread pool, or whatever resource backs the
  // `ExecutorService`, this implies that we're losing out on some potential parallelism.
  // Essentially, we're using two threads when one should suffice.
  // This is a symptom of a more serious problem with the implementation,
  // and we will discuss this later in the chapter.

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  // lift sync to async
  def asyncF[A, B](f: A => B): A => Par[B] =
    (a: A) => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight[Par[List[A]]](unit(List()))((elem, list) =>
      map2(elem, list)(_ :: _))
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {

    val fbs2: (A) => Par[List[A]] = asyncF { (x: A) =>
      if (f(x)) List(x)
      else List()
    }
    map(sequence(as map fbs2))(_.flatten)
  }

  // fold(seq, 0)(_ + _)
  def fold[A](as: IndexedSeq[A], z: A)(f: (A, A) => A): Par[A] =
    if (as.size <= 1)
      lazyUnit(as.headOption getOrElse z)
    else {
      val (l, r) = as.splitAt(as.size / 2)

      map2(fork(fold(l, z)(f)), fork(fold(r, z)(f)))(f)
    }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }

}

object Examples {

  val ec = scala.concurrent.ExecutionContext.Implicits.global

  import Par._

  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  def testMap2Future: Unit = {
    val futureOne = Par.unit(1)
    val futureTwo = Par.unit(2)

    val res = map2(futureOne, futureTwo)(_ + _)

    val res2 = run(Executors.newFixedThreadPool(5))(res).get()

    println(res2)
  }

  def testParFilter: Unit = {
    val list = List(1, 2, 3, 4, 5, 6)

    val res = parFilter(list)(_ > 2)

    println(run(Executors.newFixedThreadPool(2))(res).get())
  }

  def testFold: Unit = {
    val seq = IndexedSeq(1, 2, 3, 4, 5, 6)

    println("sum: " + run(Executors.newFixedThreadPool(10))(fold(seq, 0)(_ + _)).get())

    println("max: " + run(Executors.newFixedThreadPool(10))(fold(seq, Int.MinValue)((h, t) => if (h > t) h else t)).get())

  }
}
