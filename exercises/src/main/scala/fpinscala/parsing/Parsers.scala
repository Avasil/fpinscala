package fpinscala.parsing

import fpinscala.testing.{Gen, Prop}

import scala.language.{higherKinds, implicitConversions}
import scala.util.matching.Regex


trait Parsers[Parser[+ _]] {
  self => // so inner classes may call methods of trait


  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] =
    ParserOps[A](p)

  implicit def asStringsParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  // N powtorzen parsera np. dla "ab" = "ababab"
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n == 0)
      succeed(List())
    else
      map2(p, listOfN(n - 1, p))(_ :: _)

  //  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
  //    map2(p, p)((a, _) => List.fill(n)(a))

  // zero or more ~?
  // map2 calls product(p1, p2) and if p1 fails, p2
  // does not terminate so we end up with succeed
  def many[A](p: Parser[A]): Parser[List[A]] =
  map2(p, many(p))(_ :: _) or succeed(List[A]())

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    a flatMap (x => succeed(f(x)))

  // always succeed, something like 'unit', 'pure'
  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  // return the portion of input inspected by p if successful
  // slice('a'|'b').many("aaba") = Right(aaba)
  // ('a'|'b').many("aaba") = List(a, aa, aab, aaba)?
  def slice[A](p: Parser[A]): Parser[String]

  // 1 or more
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p1
      b <- p2
    } yield (a, b)

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p1 product p2 map (x => f(x._1, x._2))

  def map2ViaFlatMap[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- p1
      b <- p2
    } yield f(a, b)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String] =
    succeed(r.regex)

  def whitespace: Parser[String] =
    "\\s*".r

  def double: Regex =
    "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?".r

  def doubleString: Parser[Double] =
    double.map(_.toDouble)

  def boolean: Regex =
    "^(true|false)$".r

  def booleanParser: Parser[Boolean] =
    boolean.map(_.toBoolean)

  def nullString: Parser[String] =
    "^null$".r

  // value in json without trailing whitespace
  def token(p: Parser[String]) =
    p <* whitespace

  // product while skipping left
  def *>[A, B](p1: Parser[A], p2: Parser[B]): Parser[B] = for {
    _ <- slice(p1)
    b <- p2
  } yield b

  // product while skipping right
  def <*[A, B](p1: Parser[A], p2: Parser[B]): Parser[A] = for {
    a <- p1
    _ <- slice(p2)
  } yield a

  def surround[A](start: Parser[Any], end: Parser[Any])(p: => Parser[A]): Parser[A] =
    start *> p <* end

  def separate[A](token: Parser[Any])(p: => Parser[A]) =
    map2(p, many(token *> p))(_ :: _)

  def eof: Parser[String] =
    regex("\\z".r)

  def root[A](p: Parser[A]): Parser[A] =
    p <* eof

  //  def token[A](p: Parser[A]): Parser[A] =
  //    p <* whitespace

  // if p fails, ParseError will have msg
  def label[A](msg: String)(p: Parser[A]): Parser[A]


  // adds more info in case p fails
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] =
      self or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] =
      self or(p, p2)

    def many: Parser[List[A]] =
      self many p

    def map[B](f: A => B): Parser[B] =
      (self map p) (f)

    def slice: Parser[String] =
      self slice p

    def product[B](p2: Parser[B]): Parser[(A, B)] =
      self product(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] =
      p product p2

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      (self flatMap p) (f)

    def *>[B](p2: Parser[B]): Parser[B] =
      self.*>(p, p2)

    def <*[B](p2: Parser[B]): Parser[A] =
      self.<*(p, p2)

    def surround(start: Parser[Any], end: Parser[Any]): Parser[A] =
      self.surround(start, end)(p)

    def separate(token: Parser[Any]) =
      self.separate(token)(p)
  }

  object Laws {

    import fpinscala.testing.Prop.forAll

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](a: A)(in: Gen[String]): Prop =
      forAll(in)(s => run(succeed(a))(s) == Right(a))

    //    a * (b * c) == (a * b)* c
    def productAssociativeLaw[A, B, C](p1: Parser[A], p2: Parser[B], p3: Parser[C])(in: Gen[String]): Prop = {
      def unbiasL(p: ((A, B), C)): (A, B, C) = p match {
        case ((a, b), c) => (a, b, c)
      }

      def unbiasR(p: (A, (B, C))): (A, B, C) = p match {
        case (a, (b, c)) => (a, b, c)
      }

      forAll(in)(s => run(p1 ** (p2 ** p3) map unbiasR)(s) == run((p1 ** p2) ** p3 map unbiasL)(s))
    }

    def productMapLaw[A, B](p1: Parser[A], p2: Parser[B])(f: A => B, g: B => A)(in: Gen[String]): Prop =
      forAll(in)(s => run(p1.map(f) ** p2.map(g))(s) == run(p1 ** p2 map { case (a, b) => (f(a), g(b)) })(s))

    //    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
    //      forAll(inputs ** Gen.string) { case (input, msg) =>
    //        run(label(msg)(p))(input) match {
    //          case Left(e) => errorMessage(e) == msg
    //          case _ => true
    //        }
    //      }
  }

}

object Parsers {
  type Parser[+A] = String => Either[ParseError, A]
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

// stack -> wszystkie bledy?
//
case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}