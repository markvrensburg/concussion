package concussion
package compile

import cats.Monad
import cats.implicits._

trait OperandDSL[F[_]] {
  def read[A](operand: Operand[A]): F[A]
  def write[A](value: A, reference: Reference[A]): F[Unit]
}

trait NodeDSL[F[_]] {
  def getAcc[A]: F[A]
  def getBak[A]: F[A]
  def setAcc[A](value: A): F[Unit]
  def setBak[A](value: A): F[Unit]
}

trait LabelDSL[F[_]] {
  def counterAt(label: String): F[Int]
  def jump(label: String): F[Unit]
}

trait CounterDSL[F[_]] {
  def get: F[Int]
  def set(pc: Int): F[Unit]

  def inc(implicit F: Monad[F]): F[Unit] =
    get.flatMap(pc => set(pc + 1))
}
