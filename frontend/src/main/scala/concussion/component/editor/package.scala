package concussion
package component

import concussion.domain._
import japgolly.scalajs.react.Ref.Simple
import org.scalajs.dom.html

package object editor {

  type EditNode = Node[NodeMeta]
  type EditPort = Port[(PortMeta, Simple[html.Element])]

  type EditVertex = Vertex[NodeMeta, (PortMeta, Simple[html.Element])]

  type EditConnection = (EditVertex, EditVertex)

  type EditNetwork = Network[NodeMeta, (PortMeta, Simple[html.Element])]

  object EditPort {
    def apply(meta: PortMeta,
              ref: Simple[html.Element],
              name: String): EditPort =
      Port[(PortMeta, Simple[html.Element])]((meta, ref), name)
  }

  def containsId(portId: String)(connection: EditConnection): Boolean =
    (connection._1._1.meta._1.id == portId) || (connection._2._1.meta._1.id == portId)

  def connectsTo(portId: String)(connection: EditConnection): Option[EditPort] =
    connection match {
      case ((Port(meta: (PortMeta, _), _), _), (port, _))
          if meta._1.id == portId =>
        Some(port)
      case ((port, _), (Port(meta: (PortMeta, _), _), _))
          if meta._1.id == portId =>
        Some(port)
      case _ => None
    }
}
