package concussion
package component

import concussion.domain.{Connection, Network, Node, NodeMeta, Port, PortMeta}

package object editor {

  type EditNode = Node[NodeMeta]
  type EditPort = Port[PortMeta]

  type EditConnection = Connection[EditPort]

  type EditNetwork = Network[NodeMeta, PortMeta]

  object EditPort {
    def apply(meta: PortMeta, name: String): EditPort =
      Port[PortMeta](meta, name)
  }

  def containsId(portId: String)(connection: EditConnection): Boolean =
    (connection.from.meta.id == portId) || (connection.to.meta.id == portId)

  def connectsTo(portId: String)(connection: EditConnection): Option[EditPort] =
    connection match {
      case Connection(Port(meta: PortMeta, _), port) if meta.id == portId =>
        Some(port)
      case Connection(port, Port(meta: PortMeta, _)) if meta.id == portId =>
        Some(port)
      case _ => None
    }
}
