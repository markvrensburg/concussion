package concussion
package compile

import cats.effect.Concurrent
import cats.effect.concurrent.Deferred
import cats.implicits._
import fs2.Stream

//.evalMap(_ => state.get) over "run" stream for state
class Language[DSL[_]: Concurrent: OperandDSL: CounterDSL: LabelDSL: NodeDSL, A: Numeric] {

  //Read and Write: Move value from source to destination
  def mov(source: Operand[A], destination: Reference[A]): Stream[DSL, Unit] = {

    def read(cell: Deferred[DSL, A]): Stream[DSL, Unit] =
      Stream.eval(DSL.read(source).flatMap(cell.complete))

    def write(cell: Deferred[DSL, A]): Stream[DSL, Unit] =
      Stream.eval(cell.get.flatMap(DSL.write(_, destination)))

    Stream.eval(DSL.inc *> Deferred[DSL, A]).flatMap(temp => read(temp) ++ write(temp))
  }

  //Add value in register ACC to value at source and store it in register ACC
  def add(value: Operand[A]): Stream[DSL, Unit] =
    Stream.eval(for {
      _ <- DSL.inc
      acc <- DSL.getAcc[A]
      addend <- DSL.read(value)
      _ <- DSL.setAcc(A.plus(acc, addend))
    } yield ())

  //Subtract value in register ACC from value at source and store it in register ACC
  def sub(value: Operand[A]): Stream[DSL, Unit] =
    Stream.eval(for {
      _ <- DSL.inc
      acc <- DSL.getAcc[A]
      subtrahend <- DSL.read(value)
      _ <- DSL.setAcc(A.minus(acc, subtrahend))
    } yield ())

  //If register ACC has value equal to zero, jump to program counter with label
  def jez(label: String): Stream[DSL, Unit] =
    Stream.eval(for {
      acc <- DSL.getAcc[A]
      _ <- if (A.equiv(acc, A.zero)) DSL.jump(label) else DSL.inc
    } yield ())

  //If register ACC has value greater than zero, jump to program counter with label
  def jgz(label: String): Stream[DSL, Unit] =
    Stream.eval(for {
      acc <- DSL.getAcc[A]
      _ <- if (A.gt(acc, A.zero)) DSL.jump(label) else DSL.inc
    } yield ())

  //If register ACC has value less than zero, jump to program counter with label
  def jlz(label: String): Stream[DSL, Unit] =
    Stream.eval(for {
      acc <- DSL.getAcc[A]
      _ <- if (A.lt(acc, A.zero)) DSL.jump(label) else DSL.inc
    } yield ())

  //Jump to program counter with label
  def jmp(label: String): Stream[DSL, Unit] =
    Stream.eval(DSL.jump(label))

  //Jump to program counter (current + offset)
  def jro(offset: Operand[A]): Stream[DSL, Unit] =
    Stream.eval(for {
      current <- DSL.get
      off <- DSL.read(offset)
      _ <- DSL.set(current + A.toInt(off))
    } yield ())

  //Store contents of register ACC into BAK
  def sav: Stream[DSL, Unit] =
    Stream.eval(for {
      _ <- DSL.inc
      acc <- DSL.getAcc[A]
      _ <- DSL.setBak(acc)
    } yield ())

  //Swap contents of register ACC and BAK
  def swp: Stream[DSL, Unit] =
    Stream.eval(for {
      _ <- DSL.inc
      acc <- DSL.getAcc[A]
      bak <- DSL.getBak[A]
      _ <- DSL.setAcc(bak)
      _ <- DSL.setBak(acc)
    } yield ())

  //Do nothing: Increment program counter
  def nop: Stream[DSL, Unit] =
    Stream.eval(DSL.inc)
}
