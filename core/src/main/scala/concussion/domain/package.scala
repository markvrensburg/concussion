package concussion

import concussion.graph.LGraph

package object domain {

  type Network[NMeta, PMeta, Edge] = LGraph[Node[NMeta, PMeta], Edge]

}
