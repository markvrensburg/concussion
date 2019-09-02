package concussion
package graph

import cats._
import cats.implicits._

sealed trait LabelledGraph[V, E] {
  import LabelledGraph._

  def foldg[A](empty: A, vertex: V => A, connect: (E, A, A) => A): A =
    this match {
      case Empty()   => empty
      case Vertex(v) => vertex(v)
      case Connect(e, l, r) =>
        connect(e, l.foldg(empty, vertex, connect), r.foldg(empty, vertex, connect))
    }

  def foldgEval[A](empty: A, vertex: V => A, connect: (E, A, A) => A): Eval[A] =
    this match {
      case Empty()   => Eval.now(empty)
      case Vertex(v) => Eval.now(vertex(v))
      case Connect(e, l, r) =>
        val le = Eval.defer(l.foldgEval(empty, vertex, connect))
        val re = Eval.defer(r.foldgEval(empty, vertex, connect))
        (le, re).mapN(connect(e, _, _))
    }

  def +(right: LabelledGraph[V, E])(implicit ev: Monoid[E]): LabelledGraph[V, E] =
    overlay(this, right)

  def *(right: (E, LabelledGraph[V, E])): LabelledGraph[V, E] =
    connect(right._1, this, right._2)

  def isEmpty: Boolean =
    foldg[Boolean](true, _ => false, (_, vtx1, vtx2) => vtx1 && vtx2)

  def hasVertex(v: V)(implicit ev: Eq[V]): Boolean =
    foldg[Boolean](false, _ === v, (_, vtx1, vtx2) => vtx1 || vtx2)

  def toAdjacencyMap(implicit ev1: Eq[E], ev2: Monoid[E]): AdjacencyMap[V, E] =
    foldg(AdjacencyMap.empty[V, E], AdjacencyMap.vertex[V, E], AdjacencyMap.connect[V, E])

}

object LabelledGraph {

  protected[graph] final case class Empty[V, E]() extends LabelledGraph[V, E]
  protected[graph] final case class Vertex[V, E](vertex: V) extends LabelledGraph[V, E]
  protected[graph] final case class Connect[V, E](
      edge: E,
      left: LabelledGraph[V, E],
      right: LabelledGraph[V, E]
  ) extends LabelledGraph[V, E]

  def empty[V, E]: LabelledGraph[V, E] =
    Empty[V, E]()

  def vertex[V, E](vertex: V): LabelledGraph[V, E] =
    Vertex[V, E](vertex)

  def edge[V, E](edge: E, vtx1: V, vtx2: V): LabelledGraph[V, E] =
    connect(edge, vertex(vtx1), vertex(vtx2))

  def connect[V, E](
      edge: E,
      left: LabelledGraph[V, E],
      right: LabelledGraph[V, E]
  ): LabelledGraph[V, E] =
    Connect[V, E](edge, left, right)

  def overlay[V, E: Monoid](
      left: LabelledGraph[V, E],
      right: LabelledGraph[V, E]
  ): LabelledGraph[V, E] =
    connect(Monoid[E].empty, left, right)

  def vertices[V, E: Monoid](vs: Seq[V]): LabelledGraph[V, E] =
    overlays(vs.map(vertex[V, E]))

  def edges[V, E: Monoid](es: Seq[(E, V, V)]): LabelledGraph[V, E] =
    overlays(es.map(x => edge(x._1, x._2, x._3)))

  def overlays[V, E: Monoid](gs: Seq[LabelledGraph[V, E]]): LabelledGraph[V, E] =
    gs.foldLeft[LabelledGraph[V, E]](empty)(overlay)

}
