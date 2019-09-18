package concussion

import concussion.graph.Graph

package object domain {

  type Vertex[NMeta, PMeta] = (Port[PMeta], Node[NMeta])

  type Network[NMeta, PMeta] = Graph[Vertex[NMeta, PMeta]]

  type BasicNode = Node[Unit]
  type BasicPort = Port[Unit]
  type BasicNetwork = Network[Unit, Unit]

}
