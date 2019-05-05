package concussion

import atto._
import Atto._

final class Read[A](val parser: Parser[A]) {
  def parse(s: String) = parser.parseOnly(s)
}

object Read {

  @inline def apply[A](implicit P: Read[A]): Read[A] = P

  def fromParser[A](parser: Parser[A]): Read[A] =
    new Read[A](parser)

  implicit val readByte: Read[Byte] =
    Read.fromParser(byte)

  implicit val readInt: Read[Int] =
    Read.fromParser(int)

  implicit val readDouble: Read[Double] =
    Read.fromParser(double)

  implicit val readChar: Read[Char] =
    Read.fromParser(elem(_ => true))

  implicit val readString: Read[String] =
    Read.fromParser(takeWhile(!_.isWhitespace))
}
