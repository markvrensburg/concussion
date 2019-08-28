package concussion
package compile

import cats.data.{EitherNel, Reader}
import cats.implicits._

object Validation {

  def extractExecutables[A](statements: Vector[Statement[A]]): Map[Int, Opcode[A]] =
    statements.zipWithIndex.mapFilter(s => s._1.opcode.map(s._2 -> _)).toMap

  def extractLabels[A](statements: Vector[Statement[A]]): Map[String, Int] =
    statements.zipWithIndex.mapFilter(s => s._1.label.map(_.name -> s._2)).toMap

  def syncLabels[A](program: Program[A]): Program[A] =
    Program(
      program.executables,
      program.labels.mapValues { i =>
        program.executables.filter(_._1 >= i) match {
          case e if e.nonEmpty => e.minBy(_._1)._1
          case _               => i
        }
      }
    )

  def extractExecutablesR[A] = Reader(extractExecutables[A])

  def extractLabelsR[A] = Reader(extractLabels[A])

  def extract[A] = (extractExecutablesR[A], extractLabelsR[A]).tupled

  def validateLabels[A](program: Program[A]): EitherNel[String, Program[A]] = {

    def validateLabel(label: String): EitherNel[String, String] =
      if (program.labels.contains(label))
        label.rightNel
      else
        s"Label not found: $label".leftNel

    val executables = program.executables
      .map {
        case executable @ (_, jl: JumpLabel[_]) => validateLabel(jl.label).as(executable)
        case executable                         => executable.rightNel
      }
      .toList
      .sequence

    executables.map(l => Program(l.toMap, program.labels))
  }

  def program[A: Read](raw: String) =
    Parsing
      .parse[A](raw)
      .map { statements =>
        val (executables, labels) = extract.run(statements)
        Program(executables, labels)
      }
      .flatMap(validateLabels)
      .map(syncLabels)

}
