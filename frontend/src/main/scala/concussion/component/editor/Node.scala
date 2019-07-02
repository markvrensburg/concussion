package concussion.component.editor

import enum.Enum

sealed trait NodeType
case object Input extends NodeType
case object Output extends NodeType
case object Processor extends NodeType

object NodeType {
  val nodeTypes: Enum[NodeType] = Enum.derived[NodeType]
}
