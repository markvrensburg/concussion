package concussion.util

import cats.effect.IO
import concussion.component.editor._
import concussion.domain._
import concussion.geometry._
import japgolly.scalajs.react.Ref
import org.scalajs.dom.html

object Ports {

  def mkPort(node: EditNode, namer: Namer[IO]): IO[EditPort] =
    namer
      .nextName(s"${node.meta.id}_Port")
      .map(
        id =>
          EditPort(
            PortMeta(
              id,
              Anchor(
                0,
                0,
                if (Nodes.getType(node) == Input) Right
                else Left
              )
            ),
            Ref[html.Element],
            "Port"
        )
      )

  def copyPort(port: EditPort, node: EditNode, namer: Namer[IO]): IO[EditPort] =
    namer
      .nextName(s"${node.meta.id}_Port")
      .map(
        pid =>
          port.map(
            meta =>
              (
                meta._1
                  .copy(id = pid, anchor = meta._1.anchor.copy(x = 0, y = 0)),
                Ref[html.Element],
            )
        )
      )

  def shouldUpdatePorts(currentPorts: Vector[EditPort],
                        nextPorts: Vector[EditPort]): Boolean =
    (currentPorts.length != nextPorts.length) || {
      val ps1 = currentPorts
        .map(p => (p.meta._1.id, p.meta._1.anchor.orientation, p.name))
      val ps2 = nextPorts
        .map(p => (p.meta._1.id, p.meta._1.anchor.orientation, p.name))
      (ps1 != ps2)
    }
}
