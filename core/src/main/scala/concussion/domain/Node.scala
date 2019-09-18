package concussion
package domain

import enum.Enum

sealed trait NodeType

case object Input extends NodeType
case object Output extends NodeType
case object Processor extends NodeType

object NodeType {
  val nodeTypes: Enum[NodeType] = Enum.derived[NodeType]
}

sealed trait Node[Meta] {
  def meta: Meta
}
final case class InputNode[Meta](meta: Meta) extends Node[Meta]
final case class OutputNode[Meta](meta: Meta) extends Node[Meta]
final case class ProcessorNode[Meta](meta: Meta, code: String = "")
    extends Node[Meta]
