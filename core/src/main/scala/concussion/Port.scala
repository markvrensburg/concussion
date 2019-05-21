package concussion

import cats.Monad
import cats.implicits._
import cats.effect.Concurrent
import cats.effect.concurrent.{MVar, Semaphore}
import fs2.{Pipe, Stream}

abstract class PVar[F[_], A] {
  def read: F[A]
  def peek: F[Option[A]]
  def write(a: A): F[Unit]
  def isEmpty: F[Boolean]
}

object PVar {

  def create[F[_]: Concurrent,A]: F[PVar[F,A]] =
    (MVar.empty[F,A], Semaphore[F](1)).mapN((state,latch) => new PVar[F,A] {
      override def read: F[A] =
        latch.release *> state.take <* latch.release

      override def peek: F[Option[A]] =
        Monad[F].ifM(state.isEmpty)(Monad[F].pure(Option.empty), state.read.map(_.some))

      override def write(a: A): F[Unit] =
        latch.acquire *> state.put(a) <* latch.acquire

      override def isEmpty: F[Boolean] =
        state.isEmpty
    })
}

abstract class Port[F[_],A] {
  
  def read: F[A]
  def write(a: A): F[Unit]

  def source: Stream[F,A] =
    Stream.repeatEval(read)

  def sink: Pipe[F,A,Unit] =
    _.evalMap(write)
}

object Port {

  def apply[F[_],A](readPort: PVar[F,A], writePort: PVar[F,A]): Port[F,A] = new Port[F,A] {
    override def read: F[A] = readPort.read
    override def write(a: A): F[Unit] = writePort.write(a)
  }
}