package concussion
package graph

import cats._
import cats.implicits._

sealed trait LGraph[V, E] {
  import LGraph._

  def foldg[A](empty: A, vertex: V => A, connect: (E, A, A) => A): A =
    this match {
      case Empty()   => empty
      case Vertex(v) => vertex(v)
      case Connect(e, l, r) =>
        connect(
          e,
          l.foldg(empty, vertex, connect),
          r.foldg(empty, vertex, connect)
        )
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

  def +(right: LGraph[V, E])(implicit ev: Monoid[E]): LGraph[V, E] =
    overlay(this, right)

  def *(right: (E, LGraph[V, E])): LGraph[V, E] =
    connect(right._1, this, right._2)

  def isEmpty: Boolean =
    foldg[Boolean](true, _ => false, (_, vtx1, vtx2) => vtx1 && vtx2)

  def hasVertex(v: V)(implicit ev: Eq[V]): Boolean =
    foldg[Boolean](false, _ === v, (_, vtx1, vtx2) => vtx1 || vtx2)

  def toAdjacencyMap(implicit ev1: Eq[E], ev2: Monoid[E]): AdjacencyMap[V, E] =
    foldg(
      AdjacencyMap.empty[V, E],
      AdjacencyMap.vertex[V, E],
      AdjacencyMap.connect[V, E]
    )

  def induce(f: V => Boolean): LGraph[V, E] =
    foldg[LGraph[V, E]](
      empty,
      v => if (f(v)) vertex(v) else empty,
      (edge, g1, g2) => {
        (g1, g2) match {
          case (_, Empty()) => g1
          case (Empty(), _) => g2
          case _            => connect(edge, g1, g2)
        }
      }
    )

  def removeVertex(v: V)(implicit ev: Eq[V]): LGraph[V, E] =
    induce(_ =!= v)

  def focus(f: V => Boolean)(implicit ev1: Eq[E], ev2: Monoid[E]): Focus[V, E] =
    foldg[Focus[V, E]](emptyFocus, vertexFocus(f), connectFoci(_, _, _))

  def context(p: V => Boolean)(implicit ev1: Eq[E],
                               ev2: Monoid[E]): Option[Context[V, E]] = {
    val f = focus(p)
    if (f.ok)
      Some(Context(f.is, f.os))
    else
      Option.empty
  }

  def filterContext(
    v: V,
    is: V => Boolean,
    os: V => Boolean
  )(implicit ev1: Eq[V], ev2: Eq[E], ev3: Monoid[E]): LGraph[V, E] =
    context(_ === v)
      .map(c => {
        overlays[V, E](
          List(
            vertex(v),
            induce(_ =!= v),
            edges[V, E](
              c.inputs.filter(x => is(x._2)).map(x => (x._1, x._2, v))
            ),
            edges[V, E](
              c.outputs.filter(x => os(x._2)).map(x => (x._1, v, x._2))
            )
          )
        )
      })
      .getOrElse(this)

  def removeEdge(vtx1: V, vtx2: V)(implicit ev1: Eq[V],
                                   ev2: Eq[E],
                                   ev3: Monoid[E]): LGraph[V, E] =
    filterContext(vtx1, _ =!= vtx1, _ =!= vtx2)

}

object LGraph {

  protected[graph] final case class Empty[V, E]() extends LGraph[V, E]
  protected[graph] final case class Vertex[V, E](vertex: V) extends LGraph[V, E]
  protected[graph] final case class Connect[V, E](edge: E,
                                                  left: LGraph[V, E],
                                                  right: LGraph[V, E])
      extends LGraph[V, E]

  def empty[V, E]: LGraph[V, E] =
    Empty[V, E]()

  def vertex[V, E](vertex: V): LGraph[V, E] =
    Vertex[V, E](vertex)

  def edge[V, E](edge: E, vtx1: V, vtx2: V): LGraph[V, E] =
    connect(edge, vertex(vtx1), vertex(vtx2))

  def connect[V, E](edge: E,
                    left: LGraph[V, E],
                    right: LGraph[V, E]): LGraph[V, E] =
    Connect[V, E](edge, left, right)

  def overlay[V, E: Monoid](left: LGraph[V, E],
                            right: LGraph[V, E]): LGraph[V, E] =
    connect(Monoid[E].empty, left, right)

  def vertices[V, E: Monoid](vs: Seq[V]): LGraph[V, E] =
    overlays(vs.map(vertex[V, E]))

  def edges[V, E: Monoid](es: Seq[(E, V, V)]): LGraph[V, E] =
    overlays(es.map(x => edge(x._1, x._2, x._3)))

  def overlays[V, E: Monoid](gs: Seq[LGraph[V, E]]): LGraph[V, E] =
    gs.foldLeft[LGraph[V, E]](empty)(overlay)

  protected[graph] final case class Focus[V, E](ok: Boolean,
                                                is: List[(E, V)],
                                                os: List[(E, V)],
                                                vs: List[V])

  private[graph] def emptyFocus[V, E]: Focus[V, E] =
    Focus(false, List.empty, List.empty, List.empty)

  private[graph] def vertexFocus[V, E](f: V => Boolean)(v: V): Focus[V, E] =
    Focus(f(v), List.empty, List.empty, List(v))

  private[graph] def connectFoci[V, E: Eq: Monoid](
    e: E,
    x: Focus[V, E],
    y: Focus[V, E]
  ): Focus[V, E] =
    if (e === Monoid[E].empty)
      Focus(x.ok || y.ok, x.is |+| y.is, x.os |+| y.os, x.vs |+| y.vs)
    else {
      val xs = if (y.ok) x.vs.map((e, _)) else x.is
      val ys = if (x.ok) y.vs.map((e, _)) else y.os
      Focus(x.ok || y.ok, xs |+| y.is, x.os |+| ys, x.vs |+| y.vs)
    }

  protected[graph] final case class Context[V, E](inputs: List[(E, V)],
                                                  outputs: List[(E, V)])

  implicit def GraphEq[V: Eq, E: Eq: Monoid]: Eq[LGraph[V, E]] =
    (x: LGraph[V, E], y: LGraph[V, E]) => x.toAdjacencyMap === y.toAdjacencyMap

}
