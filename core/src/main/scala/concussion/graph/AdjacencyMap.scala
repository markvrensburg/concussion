package concussion
package graph

import cats._
import cats.implicits._

final case class AdjacencyMap[V, E](private val adjacencyMap: Map[V, Map[V, E]]) {

  def isEmpty: Boolean =
    adjacencyMap.isEmpty

  def hasVertex(vertex: V)(implicit ev: Order[V]): Boolean =
    adjacencyMap.exists(v => v._1 === vertex)

  def hasEdge(vtx1: V, vtx2: V)(implicit ev: Order[V]): Boolean =
    adjacencyMap.get(vtx1).exists(_.exists(v => v._1 === vtx2))

  def vertexCount: Int =
    adjacencyMap.size

  def vertexSet: Set[V] =
    adjacencyMap.keySet

  def edgeList: List[(E, V, V)] =
    for {
      (x, ys) <- adjacencyMap.toList
      (y, e) <- ys.toList
    } yield (e, x, y)

  def edgeSet: Set[(E, V, V)] =
    edgeList.toSet

  def removeVertex(vertex: V)(implicit ev: Eq[V]): AdjacencyMap[V, E] =
    AdjacencyMap(adjacencyMap.filter(_._1 =!= vertex).mapValues(_.filter(_._1 =!= vertex)))

  def removeEdge(vtx1: V, vtx2: V)(implicit ev: Eq[V]): AdjacencyMap[V, E] =
    AdjacencyMap(
      adjacencyMap.map(am => if (am._1 === vtx1) (am._1, am._2.filter(_._1 =!= vtx2)) else am)
    )

  def removeEdge(edge: E)(implicit ev: Eq[E]): AdjacencyMap[V, E] =
    AdjacencyMap(adjacencyMap.mapValues(_.filter(_._2 =!= edge)))
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

  def overlay[V, E: Eq: Monoid](l: AdjacencyMap[V, E], r: AdjacencyMap[V, E]): AdjacencyMap[V, E] =
    AdjacencyMap(l.adjacencyMap |+| r.adjacencyMap)

  def connect[V, E: Eq: Monoid](
      e: E,
      l: AdjacencyMap[V, E],
      r: AdjacencyMap[V, E]
  ): AdjacencyMap[V, E] =
    if (e === Monoid[E].empty)
      overlay(l, r)
    else {
      val targets = r.adjacencyMap.keySet.map((_, e)).toMap
      AdjacencyMap(
        l.adjacencyMap |+| (r.adjacencyMap |+| l.adjacencyMap.keySet.map((_, targets)).toMap)
      )
    }

  implicit def AdjacencyMapEq[V: Eq, E: Eq]: Eq[AdjacencyMap[V, E]] =
    Eq.by[AdjacencyMap[V, E], Map[V, Map[V, E]]](_.adjacencyMap)
}
