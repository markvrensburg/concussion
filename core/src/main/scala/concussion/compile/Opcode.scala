package concussion
package compile

sealed trait Opcode[A]

sealed trait JumpLabel[A] extends Opcode[A] {
  val label: String
}

final case class MOV[A](source: Operand[A], destination: Reference[A]) extends Opcode[A]
final case class ADD[A](value: Operand[A]) extends Opcode[A]
final case class SUB[A](value: Operand[A]) extends Opcode[A]
final case class JEZ[A](label: String) extends JumpLabel[A]
final case class JGZ[A](label: String) extends JumpLabel[A]
final case class JLZ[A](label: String) extends JumpLabel[A]
final case class JMP[A](label: String) extends JumpLabel[A]
final case class JRO[A](offset: Int) extends Opcode[A]
final case class SAV[A]() extends Opcode[A]
final case class SWP[A]() extends Opcode[A]
final case class NOP[A]() extends Opcode[A]

sealed trait Annotation

final case class Label(name: String) extends Annotation
final case class Comment(text: String) extends Annotation

final case class Statement[A](
    label: Option[Label],
    opcode: Option[Opcode[A]],
    comment: Option[Comment]
)
