package concussion.util

import cats.effect.IO
import concussion.component.editor._
import concussion.domain._
import concussion.geometry._
import japgolly.scalajs.react.Ref
import org.scalajs.dom.html

object Ports {

  def mkPort(nodeId: String,
             nodeType: NodeType,
             namer: Namer[IO]): IO[EditPort] =
    namer
      .nextName(s"${nodeId}_Port")
      .map(
        id =>
          Port(id, "Port", (nodeType match {
            case Input => Right
            case _     => Left
          }, Ref[html.Element]))
      )

  def copyPort(port: EditPort, nodeId: String, namer: Namer[IO]): IO[EditPort] =
    namer
      .nextName(s"${nodeId}_Port")
      .map(id => port.copy(id = id).mapMeta(m => (m._1, Ref[html.Element])))

  // todo use cats equals for equality
  def shouldUpdatePorts(currentPorts: NodePorts[_],
                        nextPorts: NodePorts[_]): Boolean =
    (currentPorts, nextPorts) match {
      case (Single(p1), Single(p2)) => p1 != p2
      case (Multiple(p1), Multiple(p2)) =>
        (p1.length != p1.length) || (p1 != p2)
      case _ => true
    }

}
