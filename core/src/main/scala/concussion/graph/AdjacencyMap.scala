package concussion
package graph

import cats._
import cats.implicits._

final case class AdjacencyMap[V, E](private val adjacencyMap: Map[V, Map[V, E]]) {

  def isEmpty: Boolean =
    adjacencyMap.isEmpty

  def hasVertex(vertex: V)(implicit O: Order[V]): Boolean =
    adjacencyMap.exists(v => v._1 === vertex)

  def hasEdge(vtx1: V, vtx2: V)(implicit O: Order[V]): Boolean =
    adjacencyMap.get(vtx1).exists(_.exists(v => v._1 === vtx2))
}

object AdjacencyMap {
  def empty[V, E]: AdjacencyMap[V, E] =
    AdjacencyMap(Map.empty)

  def vertex[V, E](vertex: V): AdjacencyMap[V, E] =
    AdjacencyMap(Map(vertex -> Map.empty))

  def edge[V: Order, E: Eq: Monoid](edge: E, vtx1: V, vtx2: V): AdjacencyMap[V, E] =
    if (edge === Monoid[E].empty)
      vertices(vtx1, vtx2)
    else if (vtx1 === vtx2)
      AdjacencyMap(Map(vtx1 -> Map(vtx1 -> edge)))
    else
      AdjacencyMap(Map(vtx1 -> Map(vtx2 -> edge), vtx2 -> Map.empty))

  def vertices[V: Order, E](vertex: V*): AdjacencyMap[V, E] =
    AdjacencyMap(vertex.map((_, Map.empty[V, E])).toMap)

  private def nonZeroUnion[V, E: Eq: Monoid](l: Map[V, E], r: Map[V, E]): Map[V, E] =
    (l |+| r).filter(_._2 =!= Monoid[E].empty)

  def overlay[V, E: Eq: Monoid](
      l: AdjacencyMap[V, E],
      r: AdjacencyMap[V, E]
  ): AdjacencyMap[V, E] = {

    def combine(l: Map[V, Map[V, E]], r: Map[V, Map[V, E]]): Map[V, Map[V, E]] =
      if (l.size <= r.size) {
        l.foldLeft(r) {
          case (my, (k, x)) =>
            my.updated(k, my.get(k).map(nonZeroUnion(x, _)).getOrElse(x))
        }
      } else {
        r.foldLeft(l) {
          case (mx, (k, y)) =>
            mx.updated(k, mx.get(k).map(nonZeroUnion(_, y)).getOrElse(y))
        }
      }

    AdjacencyMap(combine(l.adjacencyMap, r.adjacencyMap))
  }

  def connect[V: Order, E: Eq: Monoid](
      e: E,
      l: AdjacencyMap[V, E],
      r: AdjacencyMap[V, E]
  ): AdjacencyMap[V, E] =
    if (e === Monoid[E].empty)
      overlay(l, r)
    else {
      val targets = r.adjacencyMap.keySet.map((_, e)).toMap
      AdjacencyMap(
        nonZeroUnion(
          l.adjacencyMap,
          nonZeroUnion(r.adjacencyMap, l.adjacencyMap.keySet.map((_, targets)).toMap)
        )
      )
    }

}
