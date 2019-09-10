package concussion
package domain

import concussion.geometry.Point
import enum.Enum

sealed trait NodeType

case object Input extends NodeType
case object Output extends NodeType
case object Processor extends NodeType

object NodeType {
  val nodeTypes: Enum[NodeType] = Enum.derived[NodeType]
}

sealed trait Node
final case class InputNode(id: String, position: Point, port: Port) extends Node
final case class OutputNode(id: String, position: Point, port: Port)
    extends Node
final case class ProcessorNode(id: String,
                               position: Point,
                               code: String = "",
                               ports: List[Port])
    extends Node
