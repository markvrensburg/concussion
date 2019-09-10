package concussion.compile

import atto.Atto._
import atto._
import cats.implicits._
import cats.data.EitherNel

object Parsing {

  def whitespace: Parser[Unit] =
    takeWhile(_.isWhitespace).void

  def whitespace1: Parser[Unit] =
    takeWhile1(_.isWhitespace).void.named("Expected space")

  def separator: Parser[Unit] =
    (whitespace1 | (whitespace ~ char(',') ~ whitespace).void)
      .named("Expected separator (space or ','")

  def namedStrict: Parser[String] =
    (letter.named("Name must start with letter") ~ many(letterOrDigit).named(
      "Name must contain letters or digits"
    )).map(ref => (ref._1 :: ref._2).mkString)

  def named: Parser[String] =
    takeWhile(_.isLetterOrDigit).named("Name must contain letters or digits")

  def reference[A]: Parser[Reference[A]] =
    namedStrict.map(Reference[A])

  def immediate[A: Read]: Parser[Immediate[A]] =
    Read[A].parser.map(Immediate[A])

  def operand[A: Read]: Parser[Operand[A]] =
    immediate[A] | reference[A].widen[Operand[A]]

  def label: Parser[Label] =
    (named <* whitespace <* char(':').named("Expected colon(:)"))
      .map(Label)

  def comment: Parser[Comment] =
    (many1(char('#')) *> whitespace *> takeText)
      .named("Expected hash(#)")
      .map(Comment)

  def opcode[A: Read]: Parser[Opcode[A]] =
    letter
      .manyN(3)
      .named("Expected 3 letter opcode")
      .flatMap(_.mkString.toUpperCase match {
        case "MOV" =>
          (whitespace1 *> (operand[A] ~ (separator *> reference[A]))
            .map(op => MOV[A](op._1, op._2))).named("MOV").widen[Opcode[A]]
        case "ADD" =>
          (whitespace1 *> operand[A].map(ADD[A])).named("ADD").widen[Opcode[A]]
        case "SUB" =>
          (whitespace1 *> operand[A].map(SUB[A])).named("SUB").widen[Opcode[A]]
        case "JEZ" =>
          (whitespace1 *> named.map(JEZ[A])).named("JEZ").widen[Opcode[A]]
        case "JGZ" =>
          (whitespace1 *> named.map(JGZ[A])).named("JGZ").widen[Opcode[A]]
        case "JLZ" =>
          (whitespace1 *> named.map(JLZ[A])).named("JLZ").widen[Opcode[A]]
        case "JMP" =>
          (whitespace1 *> named.map(JMP[A])).named("JMP").widen[Opcode[A]]
        case "JRO" =>
          (whitespace1 *> operand[A].map(JRO[A])).named("JRO").widen[Opcode[A]]
        case "SAV" =>
          ok(SAV[A]())
        case "SWP" =>
          ok(SWP[A]())
        case "NOP" =>
          ok(NOP[A]())
        case opcode => err(s"Invalid opcode: $opcode")
      })

  def statement[A: Read]: Parser[Statement[A]] =
    for {
      labelOpt <- whitespace *> opt(label) <* whitespace
      opcodeOpt <- opt(opcode[A]) <* whitespace
      commentOpt <- opt(comment)
    } yield Statement(labelOpt, opcodeOpt, commentOpt)

  private def parseLine[A: Read](
    line: String
  ): EitherNel[String, Statement[A]] =
    statement.parseOnly(line) match {
      case ParseResult.Done(remain, result) if !remain.isEmpty =>
        result match {
          case Statement(_, Some(_), None) =>
            s"Unexpected input: ${remain.trim}".leftNel
          case Statement(_, None, _) =>
            opcode[A].parseOnly(remain) match {
              case ParseResult.Fail(_, stack, message) =>
                s"$stack; $message".leftNel
              case _ =>
                s"Cannot parse input: $line".leftNel
            }
          case _ =>
            s"Cannot parse input: ${line.substring(0, line.indexOf(remain))}[$remain]; $result".leftNel
        }
      case result =>
        result.either.toEitherNel
    }

  def parse[A: Read](
    program: String
  ): EitherNel[(Int, String), Vector[Statement[A]]] =
    program.lines.toVector.zipWithIndex
      .map(line => parseLine(line._1).leftMap(_.map((line._2 + 1, _))))
      .parSequence

}
