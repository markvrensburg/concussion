package concussion
package component

import concussion.domain._

package object editor {

  type EditNode = Node[NodeMeta]
  type EditPort = Port[PortMeta]

  type EditVertex = Vertex[NodeMeta, PortMeta]

  type EditConnection = (EditVertex, EditVertex)

  type EditNetwork = Network[NodeMeta, PortMeta]

  object EditPort {
    def apply(meta: PortMeta, name: String): EditPort =
      Port[PortMeta](meta, name)
  }

  def containsId(portId: String)(connection: EditConnection): Boolean =
    (connection._1._1.meta.id == portId) || (connection._2._1.meta.id == portId)

  def connectsTo(portId: String)(connection: EditConnection): Option[EditPort] =
    connection match {
      case ((Port(meta: PortMeta, _), _), (port, _)) if meta.id == portId =>
        Some(port)
      case ((port, _), (Port(meta: PortMeta, _), _)) if meta.id == portId =>
        Some(port)
      case _ => None
    }
}
