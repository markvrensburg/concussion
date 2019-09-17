package concussion
package component

import concussion.domain.{Network, Node, Port, Connection}

package object editor {

  type EditNode = Node[NodeMeta, PortMeta]
  type EditPort = Port[PortMeta]

  type EditConnection = Connection[PortMeta]

  type EditNetwork = Network[NodeMeta, PortMeta]

  object EditPort {
    def apply(meta: PortMeta, name: String): EditPort =
      Port[PortMeta](meta, name)
  }
}
