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

sealed trait Node[NMeta, PMeta] {
  import Node._

  val id: String
  val meta: NMeta
  val ports: NodePorts[PMeta]

  def mapMeta[B](f: NMeta => B): Node[B, PMeta] = this match {
    case n @ InputNode(_, meta, _)        => n.copy(meta = f(meta))
    case n @ OutputNode(_, meta, _)       => n.copy(meta = f(meta))
    case n @ ProcessorNode(_, _, meta, _) => n.copy(meta = f(meta))
  }

  def mapPorts[B](f: NodePorts[PMeta] => NodePorts[B]): Node[NMeta, B] =
    this match {
      case n @ InputNode(_, _, ports)        => n.copy(ports = f(ports))
      case n @ OutputNode(_, _, ports)       => n.copy(ports = f(ports))
      case n @ ProcessorNode(_, _, _, ports) => n.copy(ports = f(ports))
    }
}

object Node {
  final case class InputNode[NMeta, PMeta](id: String,
                                           meta: NMeta,
                                           ports: NodePorts[PMeta])
      extends Node[NMeta, PMeta]

  final case class OutputNode[NMeta, PMeta](id: String,
                                            meta: NMeta,
                                            ports: NodePorts[PMeta])
      extends Node[NMeta, PMeta]

  final case class ProcessorNode[NMeta, PMeta](id: String,
                                               code: String = "",
                                               meta: NMeta,
                                               ports: NodePorts[PMeta])
      extends Node[NMeta, PMeta]
}
