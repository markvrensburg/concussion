package concussion
package compile

import atto.Atto._
import atto._

final class Read[A](val parser: Parser[A]) extends AnyVal {
  def parse(s: String): Either[String, A] = parser.parseOnly(s).either
}

object Read extends ReadPriority0 {

  @inline final def apply[A](implicit ev: Read[A]): Read[A] = ev

  implicit val readLong: Read[Long] =
    fromParser(long)
}

trait ReadPriority0 extends ReadUtil {

  implicit val readDouble: Read[Double] =
    fromParser(double)

  implicit val readInt: Read[Int] =
    fromParser(int)

  implicit val readByte: Read[Byte] =
    fromParser(byte)

  implicit val readChar: Read[Char] =
    fromParser(elem(_ => true))

  implicit val readString: Read[String] =
    fromParser(takeWhile(!_.isWhitespace))
}

trait ReadUtil {

  def fromParser[A](parser: Parser[A]): Read[A] =
    new Read[A](parser)
}
