package concussion

import concussion.graph.Graph

package object domain {

  type Network[NMeta, PMeta] = Graph[(Port[PMeta], Node[NMeta])]

  type BasicNode = Node[Unit]
  type BasicPort = Port[Unit]
  type BasicNetwork = Network[Unit, Unit]

}
