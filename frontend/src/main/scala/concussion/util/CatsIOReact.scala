package concussion
package util

import cats.~>
import cats.effect.IO
import japgolly.scalajs.react.CallbackTo

object CatsIOReact {

  implicit val IOToCallback: IO ~> CallbackTo = new ~>[IO, CallbackTo] {
    override def apply[A](io: IO[A]): CallbackTo[A] = CallbackTo(io.unsafeRunSync())
  }

}
