package concussion
package util

import cats.effect.IO
import concussion.component.editor._
import concussion.domain._
import concussion.geometry._

object Nodes {

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
}
