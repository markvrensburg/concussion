import org.scalatest._
import atto._
import Atto._
import atto.ParseResult._
import cats.data.Validated.Valid
import concussion._
import concussion.compile.{Comment, Immediate, Label, Reference, Statement}
import concussion.compile.Parsing._

class ParsingSpec extends FunSuite {

  test("MOV should parse correctly") {
    assert(
      opcode[Byte].parseOnly("MOV 8  ACC.4") == Done(".4", MOV(Immediate(8), Reference("ACC")))
    )
  }

  test("MOV should not parse") {
    assert(
      opcode[Byte].parseOnly("MOV ACC 3") == Fail("MOV ACC 3", List(), "Error parsing opcode")
    )
  }

  test("Parse comment") {
    assert(
      parse[Byte]("# comment") == Valid(
        Vector(Statement(None, None, Some(Comment("comment"))))
      )
    )
  }

  test("Parse label") {
    assert(
      parse[Byte]("label:") == Valid(
        Vector(Statement(Some(Label("label")), None, None))
      )
    )
  }

  test("Parse statement") {
    assert(
      parse[Byte]("label: mov 8 acc #comment") == Valid(
        Vector(Statement(Some(Label("label")), None, None))
      )
    )
  }

  test("Parse program") {
    assert(
      parse[Byte]("MOV 8 ACC #mov") == Valid(
        Vector(Statement(None, Some(MOV(Immediate(8), Reference("ACC"))), None))
      )
    )
  }

  test("Parse program fail") {
    assert(
      parse[Byte]("MOV 8 7\nMOV ACC") == Valid(
        Vector(Statement(None, Some(MOV(Immediate(8), Reference("ACC"))), None))
      )
    )
  }
}
