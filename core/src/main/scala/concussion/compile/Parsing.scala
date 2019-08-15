package concussion
package compile

import atto.Atto._
import atto._
import cats.implicits._

object Parsing {

  private def asParseError[A](p: ParseResult[A], in: String): Either[ParseError, A] =
    p match {
      case ParseResult.Done(output, result) =>
        if (output.isEmpty)
          Right(result)
        else
          Left(
            ParseError(
              s"Cannot parse excess input: ${in.substring(0, in.indexOf(output))}[$output]"
            )
          )
      case ParseResult.Partial(_) => Left(ParseError("unexpected end-of-line"))
      case ParseResult.Fail(input, _, message) =>
        Left(ParseError(s"message: $message; input: $input"))
    }

  def whitespace: Parser[Unit] =
    takeWhile(_.isWhitespace).void

  def whitespace1: Parser[Unit] =
    takeWhile1(_.isWhitespace).void

  def separator: Parser[Unit] =
    whitespace1 | (whitespace ~ char(',') ~ whitespace).void

  def named: Parser[String] =
    takeWhile(c => c.isLetterOrDigit || c == '_')
      .named("Name must contain letters, digits or underscores")

  def namedStrict: Parser[String] =
    (letter.named("Name must start with letter") ~ opt(named))
      .map(ref => ref._1 + ref._2.getOrElse(""))

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

  def MOVparser[A: Read]: Parser[Opcode[A]] =
    (stringCI("MOV") *> whitespace1 *> (operand[A] ~ (separator *> reference[A])))
      .map(op => MOV[A](op._1, op._2))

  def ADDparser[A: Read]: Parser[Opcode[A]] =
    (stringCI("ADD") *> whitespace1 *> operand[A]).map(ADD[A])

  def SUBparser[A: Read]: Parser[Opcode[A]] =
    (stringCI("SUB") *> whitespace1 *> operand[A]).map(SUB[A])

  def JEZparser[A]: Parser[Opcode[A]] =
    (stringCI("JEZ") *> whitespace1 *> named).map(JEZ[A])

  def JGZparser[A]: Parser[Opcode[A]] =
    (stringCI("JGZ") *> whitespace1 *> named).map(JGZ[A])

  def JLZparser[A]: Parser[Opcode[A]] =
    (stringCI("JLZ") *> whitespace1 *> named).map(JLZ[A])

  def JMPparser[A]: Parser[Opcode[A]] =
    (stringCI("JMP") *> whitespace1 *> named).map(JMP[A])

  def JROparser[A: Read]: Parser[Opcode[A]] =
    (stringCI("JRO") *> whitespace1 *> int).map(JRO[A])

  def SAVparser[A]: Parser[Opcode[A]] =
    stringCI("SAV").as(SAV[A]())

  def SWPparser[A]: Parser[Opcode[A]] =
    stringCI("SWP").as(SWP[A]())

  def NOPparser[A]: Parser[Opcode[A]] =
    stringCI("NOP").as(NOP[A]())

  def opcode[A: Read]: Parser[Opcode[A]] =
    List(
      MOVparser[A],
      ADDparser[A],
      SUBparser[A],
      JEZparser[A],
      JGZparser[A],
      JLZparser[A],
      JMPparser[A],
      JROparser[A],
      SAVparser[A],
      SWPparser[A],
      NOPparser[A],
      err[Opcode[A]]("Error parsing opcode")
      //todo return Validated with better error messages (Error ADT perhaps)
    ).combineAll

  def statement[A: Read]: Parser[Statement[A]] =
    for {
      labelOpt <- whitespace *> opt(label) <* whitespace
      opcodeOpt <- opt(opcode[A]) <* whitespace
      commentOpt <- opt(comment)
    } yield Statement(labelOpt, opcodeOpt, commentOpt)

  def lines: Parser[Vector[String]] = takeText.map(_.lines.toVector)

  def parseAll[A](parser: Parser[A])(input: String): Either[List[CompileError], A] =
    asParseError(parser.parseOnly(input), input).leftMap(List(_))

  def parse[A: Read](program: String): Either[List[CompileError], Vector[Statement[A]]] =
    program.lines.toVector.map(parseAll(statement[A])).parSequence
}
