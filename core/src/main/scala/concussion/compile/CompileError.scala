package concussion
package compile

sealed trait CompileError

final case class ParseError(message: String) extends CompileError
final case class UnexpectedEndOfLine(opcode: String) extends CompileError
final case class ExpectedParameters(opcode: String) extends CompileError
final case class InvalidStatement(statement: String) extends CompileError
final case class InvalidReference(reference: String) extends CompileError
final case class NoLabelFound(label: String) extends CompileError
final case class DuplicatePortName(name: String) extends CompileError
