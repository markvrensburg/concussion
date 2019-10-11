package concussion.component.editor

import cats.kernel.{Eq, Monoid}
import concussion.geometry.Anchor

final case class ConnectionPoint(nodeId: String, portId: String, anchor: Anchor)

final case class Connection(from: ConnectionPoint, to: ConnectionPoint)

final case class Connections(connections: Set[Connection]) {

  def containsConnection(portId: String): Boolean =
    connections.exists(
      c => (c.from.portId == portId) || (c.to.portId == portId)
    )

  def removeConnection(portId: String): Connections =
    Connections(
      connections
        .filter(c => (c.from.portId != portId) && (c.to.portId == portId))
    )
}

object Connections {

  implicit val connectionEq: Eq[Connections] = Eq.fromUniversalEquals

  implicit val connectionMonoid: Monoid[Connections] = new Monoid[Connections] {
    override def empty: Connections =
      Connections(Set.empty)

    override def combine(x: Connections, y: Connections): Connections =
      Connections(x.connections.union(y.connections))
  }
}

//  def containsId(portId: String)(connection: EditConnection): Boolean =
//    (connection._1._1.meta._1.id == portId) || (connection._2._1.meta._1.id == portId)
//
//  def connectsTo(portId: String)(connection: EditConnection): Option[EditPort] =
//    connection match {
//      case ((Port(meta: (PortMeta, _), _), _), (port, _))
//          if meta._1.id == portId =>
//        Some(port)
//      case ((port, _), (Port(meta: (PortMeta, _), _), _))
//          if meta._1.id == portId =>
//        Some(port)
//      case _ => None
//    }
