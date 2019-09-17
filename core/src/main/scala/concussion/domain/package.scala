package concussion

import concussion.graph.Graph

package object domain {

  type Network[NodeMeta, PortMeta] =
    Graph[Node[NodeMeta, PortMeta], List[Connection[PortMeta]]]

  type BasicNode = Node[Unit, Unit]
  type BasicPort = Port[Unit]
  type BasicNetwork = Network[Unit, Unit]

}
