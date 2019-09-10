package concussion

import concussion.graph.Graph

package object domain {

  type Network = Graph[Node, List[Connection]]

}
