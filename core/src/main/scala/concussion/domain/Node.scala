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

sealed trait Node[Meta, PortMeta]
final case class InputNode[Meta, PortMeta](meta: Meta, port: Port[PortMeta])
    extends Node[Meta, PortMeta]
final case class OutputNode[Meta, PortMeta](meta: Meta, port: Port[PortMeta])
    extends Node[Meta, PortMeta]
final case class ProcessorNode[Meta, PortMeta](meta: Meta,
                                               code: String = "",
                                               ports: List[Port[PortMeta]])
    extends Node[Meta, PortMeta]
