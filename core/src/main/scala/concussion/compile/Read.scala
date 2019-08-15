package concussion
package compile

import atto.Atto._
import atto._

final class Read[A](val parser: Parser[A]) {
  def parse(s: String): Either[String, A] = parser.parseOnly(s).either
}

object Read {

  @inline final def apply[A](implicit ev: Read[A]): Read[A] = ev

  def fromParser[A](parser: Parser[A]): Read[A] =
    new Read[A](parser)

  implicit val readByte: Read[Byte] =
    Read.fromParser(byte)

  implicit val readInt: Read[Int] =
    Read.fromParser(int)

  implicit val readLong: Read[Long] =
    Read.fromParser(long)

  implicit val readDouble: Read[Double] =
    Read.fromParser(double)

  implicit val readChar: Read[Char] =
    Read.fromParser(elem(_ => true))

  implicit val readString: Read[String] =
    Read.fromParser(takeWhile(!_.isWhitespace))
}
