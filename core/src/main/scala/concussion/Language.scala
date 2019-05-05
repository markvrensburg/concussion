package concussion

import cats.implicits._
import cats.effect.Concurrent
import cats.effect.concurrent.Deferred
import fs2.Stream

//.evalMap(_ => state.get) over "run" stream for state
class Language[F[_]: Concurrent,A](implicit numeric: Numeric[A],
                                   operandDSL: OperandDSL[F,A],
                                   counterDSL: CounterDSL[F],
                                   labelDSL: LabelDSL[F],
                                   nodeDSL: NodeDSL[F,A]) {

  //Read and Write: Move value from source to destination
  def mov(source: Operand[A], destination: Reference[A]): Stream[F,Unit] = {

    def read(cell: Deferred[F, A]) =
      Stream.eval(operandDSL.read(source).flatMap(cell.complete))

    def write(cell: Deferred[F, A]) =
      Stream.eval(cell.get.flatMap(operandDSL.write(_, destination)))

    Stream.eval(counterDSL.inc *> Deferred[F, A]).flatMap(temp => read(temp) ++ write(temp))
  }

  //Add value in register ACC to value at source and store it in register ACC
  def add(value: Operand[A]): Stream[F,Unit] =
    Stream.eval(for {
      _ <- counterDSL.inc
      acc <- nodeDSL.getAcc
      addend <- operandDSL.read(value)
      _ <- nodeDSL.setAcc(numeric.plus(acc,addend))
    } yield ())

  //Subtract value in register ACC from value at source and store it in register ACC
  def sub(value: Operand[A]): Stream[F,Unit] =
    Stream.eval(for {
      _ <- counterDSL.inc
      acc <- nodeDSL.getAcc
      subtrahend <- operandDSL.read(value)
      _ <- nodeDSL.setAcc(numeric.minus(acc,subtrahend))
    } yield ())

  //If register ACC has value equal to zero, jump to program counter with label
  def jez(label: Immediate[String]): Stream[F,Unit] =
    Stream.eval(for {
      acc <- nodeDSL.getAcc
      _ <- if (numeric.equiv(acc,numeric.zero)) labelDSL.jump(label.value) else counterDSL.inc
    } yield ())

  //If register ACC has value greater than zero, jump to program counter with label
  def jgz(label: Immediate[String]): Stream[F,Unit] =
    Stream.eval(for {
      acc <- nodeDSL.getAcc
      _ <- if (numeric.gt(acc,numeric.zero)) labelDSL.jump(label.value) else counterDSL.inc
    } yield ())

  //If register ACC has value less than zero, jump to program counter with label
  def jlz(label: Immediate[String]): Stream[F,Unit] =
    Stream.eval(for {
      acc <- nodeDSL.getAcc
      _ <- if (numeric.lt(acc,numeric.zero)) labelDSL.jump(label.value) else counterDSL.inc
    } yield ())

  //Jump to program counter with label
  def jmp(label: Immediate[String]): Stream[F,Unit] =
    Stream.eval(labelDSL.jump(label.value))

  //Jump to program counter (current + offset)
  def jro(offset: Immediate[Int]): Stream[F,Unit] =
    Stream.eval(for {
      current <- counterDSL.get
      _ <- counterDSL.set(current + offset.value)
    } yield ())

  //Store contents of register ACC into BAK
  def sav: Stream[F,Unit] =
    Stream.eval(for {
      _ <- counterDSL.inc
      acc <- nodeDSL.getAcc
      _ <- nodeDSL.setBak(acc)
    } yield ())

  //Swap contents of register ACC and BAK
  def swp: Stream[F,Unit] =
    Stream.eval(for {
      _ <- counterDSL.inc
      acc <- nodeDSL.getAcc
      bak <- nodeDSL.getBak
      _ <- nodeDSL.setAcc(bak)
      _ <- nodeDSL.setBak(acc)
    } yield ())

  //Do nothing: Increment program counter
  def nop: Stream[F,Unit] =
    Stream.eval(counterDSL.inc)
}