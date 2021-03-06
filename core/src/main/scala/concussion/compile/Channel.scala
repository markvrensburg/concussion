package concussion
package compile

import cats.effect.Concurrent
import cats.effect.concurrent.{MVar, Semaphore}
import cats.implicits._
import fs2.{Pipe, Stream}

abstract class CVar[F[_], A] {
  def read: F[A]
  def write(a: A): F[Unit]
  def isEmpty: F[Boolean]
}

object CVar {

  def create[F[_]: Concurrent, A]: F[CVar[F, A]] =
    (MVar.empty[F, A], Semaphore[F](1)).mapN(
      (state, latch) =>
        new CVar[F, A] {
          override def read: F[A] =
            latch.release *> state.take <* latch.release

          override def write(a: A): F[Unit] =
            latch.acquire *> state.put(a) <* latch.acquire

          override def isEmpty: F[Boolean] =
            state.isEmpty
        }
    )
}

abstract class Channel[F[_], A] {

  def read: F[A]
  def write(a: A): F[Unit]

  def source: Stream[F, A] =
    Stream.repeatEval(read)

  def sink: Pipe[F, A, Unit] =
    _.evalMap(write)
}

object Channel {

  def apply[F[_], A](readCV: CVar[F, A], writeCV: CVar[F, A]): Channel[F, A] =
    new Channel[F, A] {
      override def read: F[A] = readCV.read
      override def write(a: A): F[Unit] = writeCV.write(a)
    }

  def connection[F[_], A](cv1: CVar[F, A], cv2: CVar[F, A]): (Channel[F, A], Channel[F, A]) =
    (Channel(cv1, cv2), Channel(cv2, cv1))
}
