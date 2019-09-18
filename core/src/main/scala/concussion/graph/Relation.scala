package concussion
package graph

final case class Relation[A](domain: Set[A], relation: Set[(A, A)])

object Relation {

  def fromGraph[A](graph: Graph[A]): Relation[A] =
    Graph.foldg[A, Relation[A]](
      Relation(Set.empty, Set.empty),
      v => Relation(Set(v), Set.empty),
      (r1, r2) =>
        Relation(r1.domain.union(r2.domain), r1.relation.union(r2.relation)),
      (r1, r2) =>
        Relation(
          r1.domain.union(r2.domain),
          r1.relation
            .union(r2.relation)
            .union(r1.domain.flatMap(x => r2.domain.map((x, _))))
      ),
    )(graph)
}
