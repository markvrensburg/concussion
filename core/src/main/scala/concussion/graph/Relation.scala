package concussion
package graph

import concussion.graph.Graph.create

final case class Relation[A] private (domain: Set[A], relation: Set[(A, A)]) {

  def toGraph: Graph[A] =
    Relation.toGraph(this)
}

object Relation {

  def empty[A]: Relation[A] = Relation(Set.empty[A], Set.empty[(A, A)])

  def vertex[A](v: A): Relation[A] = Relation(Set(v), Set.empty[(A, A)])

  def overlay[A](g1: Relation[A], g2: Relation[A]): Relation[A] =
    Relation(g1.domain.union(g2.domain), g1.relation.union(g2.relation))

  def connect[A](g1: Relation[A], g2: Relation[A]): Relation[A] =
    Relation(
      g1.domain.union(g2.domain),
      g1.relation
        .union(g2.relation)
        .union(g1.domain.flatMap(x => g2.domain.map((x, _))))
    )

  def fromGraph[A](graph: Graph[A]): Relation[A] =
    Graph.foldg[A, Relation[A]](empty, vertex, overlay, connect)(graph)

  def toGraph[A](relation: Relation[A]): Graph[A] =
    create(relation.domain.toList, relation.relation.toList)
}
