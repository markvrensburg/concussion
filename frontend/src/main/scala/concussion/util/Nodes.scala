package concussion
package util

import cats.implicits._
import cats.effect.IO
import concussion.component.editor._
import concussion.domain._
import Node._
import cats.data.Chain
import japgolly.scalajs.react.Ref
import org.scalajs.dom.html

object Nodes {

  def getType(node: Node[_, _]): NodeType = node match {
    case InputNode(_, _, _)        => Input
    case OutputNode(_, _, _)       => Output
    case ProcessorNode(_, _, _, _) => Processor
  }

  def mkNode(nodeType: NodeType, namer: Namer[IO]): IO[EditNode] =
    nodeType match {
      case Input =>
        for {
          nodeId <- namer.nextName(NodeType.nodeTypes.encode(nodeType))
          port <- Ports.mkPort(nodeId, nodeType, namer)
        } yield InputNode(nodeId, Ref[html.Element], Single(port))
      case Output =>
        for {
          nodeId <- namer.nextName(NodeType.nodeTypes.encode(nodeType))
          port <- Ports.mkPort(nodeId, nodeType, namer)
        } yield OutputNode(nodeId, Ref[html.Element], Single(port))
      case Processor =>
        for {
          nodeId <- namer.nextName(NodeType.nodeTypes.encode(nodeType))
          port <- Ports.mkPort(nodeId, nodeType, namer)
        } yield
          ProcessorNode(nodeId, "", Ref[html.Element], Multiple(Chain(port)))
    }

  def copyNode(node: EditNode, namer: Namer[IO]): IO[EditNode] =
    for {
      nodeId <- namer.nextName(NodeType.nodeTypes.encode(getType(node)))
      ports <- node.ports match {
        case Single(port) => Ports.copyPort(port, nodeId, namer).map(Single(_))
        case Multiple(ports) =>
          ports
            .traverse(port => Ports.copyPort(port, nodeId, namer))
            .map(Multiple(_))
      }
    } yield
      node match {
        case InputNode(_, _, _)  => InputNode(nodeId, Ref[html.Element], ports)
        case OutputNode(_, _, _) => OutputNode(nodeId, Ref[html.Element], ports)
        case ProcessorNode(_, code, _, _) =>
          ProcessorNode(nodeId, code, Ref[html.Element], ports)
      }

  def shouldUpdateNode(currentNode: Node[_, _], nextNode: Node[_, _]): Boolean =
    (currentNode.id != nextNode.id) /*|| {
      (currentNode, nextNode) match {
        case (ProcessorNode(_, code1,_,_), ProcessorNode(_, code2,_,_)) =>
          code1 != code2
        case _ => false
      }
    }*/
}
