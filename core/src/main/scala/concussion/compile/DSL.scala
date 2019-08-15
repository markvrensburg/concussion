package concussion
package compile

import cats.Monad
import cats.implicits._

import scala.language.higherKinds

trait OperandDSL[F[_], A] {
  def read(operand: Operand[A]): F[A]
  def write(value: A, reference: Reference[A]): F[Unit]
}

trait NodeDSL[F[_], A] {
  def getAcc: F[A]
  def getBak: F[A]
  def setAcc(value: A): F[Unit]
  def setBak(value: A): F[Unit]
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
