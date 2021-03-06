package concussion
package util

import cats.effect.IO
import concussion.component.editor._
import concussion.domain._
import concussion.geometry._
import Node._

object Nodes {

  def getType(node: Node[_]): NodeType = node match {
    case InputNode(_)        => Input
    case OutputNode(_)       => Output
    case ProcessorNode(_, _) => Processor
  }

  def mkNode(nodeType: NodeType, namer: Namer[IO]): IO[EditNode] =
    nodeType match {
      case Input =>
        namer
          .nextName(NodeType.nodeTypes.encode(nodeType))
          .map(id => InputNode(NodeMeta(id, Point(0, 0))))
      case Output =>
        namer
          .nextName(NodeType.nodeTypes.encode(nodeType))
          .map(id => OutputNode(NodeMeta(id, Point(0, 0))))
      case Processor =>
        namer
          .nextName(NodeType.nodeTypes.encode(nodeType))
          .map(id => ProcessorNode(NodeMeta(id, Point(0, 0)), ""))
    }

  def copyNode(node: EditNode, namer: Namer[IO]): IO[EditNode] =
    namer
      .nextName(NodeType.nodeTypes.encode(getType(node)))
      .map(nid => node.map(_.copy(id = nid)))

  def shouldUpdateNode(currentNode: EditNode, nextNode: EditNode): Boolean =
    (currentNode.meta.id != nextNode.meta.id) /* || {
      (currentNode, nextNode) match {
        case (ProcessorNode(_, code1), ProcessorNode(_, code2)) =>
          code1 != code2
        case _ => false
      }
    } */
}
