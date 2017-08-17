package fpinscala.parsing

import fpinscala.parsing.MyParserTypes.Parser

object MyParserTypes {
  type Parser[+A] = Location => Result[A]

  sealed trait Result[+A]

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError) extends Result[Nothing]


}

object MyParsers extends Parsers[Parser] {
  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

  override implicit def string(s: String): Parser[String] = ???

  override def slice[A](p: Parser[A]): Parser[String] = ???

  override def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] = ???

  override def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def attempt[A](p: Parser[A]): Parser[A] = ???
}