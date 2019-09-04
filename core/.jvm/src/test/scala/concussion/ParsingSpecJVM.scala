package concussion

import org.scalatest._
import atto._
import Atto._
import atto.ParseResult._
import cats.implicits._
import concussion.compile._
import concussion.compile.Parsing._
import scala.reflect.runtime.{universe => ru}

class ParsingSpecJVM extends FunSuite {

  private val opcodeClazz = ru.typeOf[Opcode[Any]].typeSymbol.asClass
  private val jumpLabelClazz = ru.typeOf[JumpLabel[Any]].typeSymbol.asClass
  private val opcodes1: List[String] =
    opcodeClazz.knownDirectSubclasses.toList
      .map(_.name.toString)
      .filter(_ != "JumpLabel")
  private val opcodes2: List[String] =
    jumpLabelClazz.knownDirectSubclasses.toList
      .map(_.name.toString)
  private val opcodes: List[String] = opcodes1 ++ opcodes2

  test("Opcode: Opcodes are 3 letters only") {
    opcodes.foreach(op => assert(op.length == 3))
  }

  test("Opcode: Enumerate all opcodes in parser") {
    opcodes.foreach(
      op =>
        assert(opcode[Byte].parseOnly(op) match {
          case Done(_, _)                                       => true
          case Fail(_, _, msg) if msg != s"Invalid opcode: $op" => true
          case _                                                => false
        }, s"**Unhandled opcode $op")
    )
  }
}
