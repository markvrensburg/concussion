package concussion
package compile

sealed trait Operand[A]

final case class Reference[A](name: String) extends Operand[A]

final case class Immediate[A](value: A) extends Operand[A]
