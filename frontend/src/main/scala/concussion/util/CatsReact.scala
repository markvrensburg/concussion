package concussion.util

import cats.Eq
import cats.effect.IO
import japgolly.scalajs.react.{CallbackTo, Reusability}

object CatsReact {

  final class IOOps[A](private val io: IO[A]) extends AnyVal {
    def toCallback: CallbackTo[A] =
      CallbackTo(io.unsafeRunSync())
  }

  final class ReusabilityOps(private val ε: Reusability.type) extends AnyVal {
    def byEq[A](implicit eq: Eq[A]): Reusability[A] =
      new Reusability[A](eq.eqv)

    def byRefOrEq[A <: AnyRef: Eq]: Reusability[A] =
      Reusability.byRef[A] || byEq[A]
  }

  implicit final def CatsReact_Reusability(a: Reusability.type) = new ReusabilityOps(a)

  implicit final def CatsReact_IOOps[A](io: IO[A]) = new IOOps(io)
}
