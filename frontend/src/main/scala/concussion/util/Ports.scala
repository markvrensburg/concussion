package concussion.util

import cats.effect.IO
import concussion.component.editor._
import concussion.domain._
import concussion.geometry._

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
            "Port"
        )
      )

  def copyPort(port: EditPort, node: EditNode, namer: Namer[IO]): IO[EditPort] =
    namer
      .nextName(s"${node.meta.id}_Port")
      .map(
        pid =>
          port.map(
            meta => meta.copy(id = pid, anchor = meta.anchor.copy(x = 0, y = 0))
        )
      )
}
