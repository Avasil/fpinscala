package fpinscala.parsing

import scala.language.higherKinds

trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    //    implicit def tok(s: String) = token(P.string(s))


    val spaces = char(' ').many.slice

    def literal: Parser[JSON] = doubleString.map(JNumber) |
      booleanParser.map(JBool) |
      nullString.map(_ => JNull)

    // to co moze byc po :
    def value: Parser[JSON] = literal | obj | array

    // to co przed :
    def key: Parser[(String, JSON)] = literal ** (":" *> value)

    def array: Parser[JArray] =
      value separate "," surround('[', ']') map (a => JArray(a.toIndexedSeq))

    def obj: Parser[JObject] =
      key separate "," surround('{', '}') map (a => JObject(a.toMap))

    root(whitespace *> (obj | array))
  }

  //  def nullParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JNull] = {
  //
  //  }

  def numberParser[Err, Parser[+ _]](P: Parsers[Err, Parser]): Parser[JNumber] = {

  }
}
