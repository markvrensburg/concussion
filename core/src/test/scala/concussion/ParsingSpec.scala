package concussion

import atto.Atto._
import atto.ParseResult._
import atto._
import cats.data.NonEmptyList
import cats.implicits._
import concussion.compile.Parsing._
import concussion.compile._
import org.scalatest._

class ParsingSpec extends FunSuite {

  test("Opcode: MOV should parse") {
    assert(
      opcode[Byte]
        .parseOnly("MOV 42 ACC") == Done(
        "",
        MOV(Immediate(42), Reference("ACC"))
      )
    )
  }

  test("Opcode: MOV should not parse") {
    assert(
      opcode[Byte].parseOnly("MOV ACC 3") == Fail(
        "3",
        List("MOV", "Name must start with letter"),
        "Failure reading:letter"
      )
    )
  }

  test("Opcode: JRO should parse") {
    assert(opcode[Byte].parseOnly("JRO 42") == Done("", JRO(Immediate(42))))
  }

  test("Opcode: JRO should not parse") {
    assert(
      opcode[Byte].parseOnly("JRO") == Fail(
        "",
        List("JRO", "Expected space"),
        "not enough bytes"
      )
    )
  }

  test("Opcode: JMP should parse") {
    assert(opcode[Byte].parseOnly("JMP 42") == Done("", JMP("42")))
  }

  test("Opcode: JMP should not parse") {
    assert(
      opcode[Byte].parseOnly("JMP") == Fail(
        "",
        List("JMP", "Expected space"),
        "not enough bytes"
      )
    )
  }

  test("Opcode: Invalid opcode should not parse") {
    assert(
      opcode[Byte]
        .parseOnly("MAV ACC 3") == Fail(" ACC 3", List(), "Invalid opcode: MAV")
    )
  }

  test("Opcode: Incomplete opcode should not parse") {
    assert(
      opcode[Byte].parseOnly("MO ACC 3") == Fail(
        " ACC 3",
        List("Expected 3 letter opcode", "ManyN(3, letter)"),
        "Failure reading:letter"
      )
    )
  }

  test("Comment: comment should parse") {
    assert(comment.parseOnly("# comment") == Done("", Comment("comment")))
  }

  test("Comment: comment should not parse") {
    assert(
      comment.parseOnly("comment") == Fail(
        "comment",
        List("Expected hash(#)"),
        "Failure reading:'#'"
      )
    )
  }

  test("Label: label should parse") {
    assert(label.parseOnly("label:") == Done("", Label("label")))
  }

  test("Label: label should not parse") {
    assert(
      label.parseOnly("label") == Fail(
        "",
        List("Expected colon(:)"),
        "Failure reading:':'"
      )
    )
  }

  test("Statement: statement should parse") {
    assert(
      parse[Byte]("label: mov 8 acc #comment") == Vector(
        Statement(
          Some(Label("label")),
          Some(MOV(Immediate(8), Reference("acc"))),
          Some(Comment("comment"))
        )
      ).rightNel
    )
  }

  test("Statement: statement should not parse") {
    assert(
      parse[Byte]("label mov 8 acc #comment") == (
        1,
        "List(); Invalid opcode: LAB"
      ).leftNel
    )
  }

  test("Program: program should parse") {
    assert(
      parse[Byte]("MOV 42 ACC\nADD 42") == Vector(
        Statement(None, Some(MOV(Immediate(42), Reference("ACC"))), None),
        Statement(None, Some(ADD(Immediate(42))), None)
      ).rightNel
    )
  }

  test("Program: program should not parse") {
    assert(
      parse[Byte]("MOV 42 42\nADD 42\nSAV A") == NonEmptyList
        .of(
          (1, "List(MOV, Name must start with letter); Failure reading:letter"),
          (3, "Unexpected input: A")
        )
        .asLeft
    )
  }
}
