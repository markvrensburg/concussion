package concussion
package graph

import cats._
import cats.implicits._

sealed trait Graph[V] {
  import Graph._

  def +(right: Graph[V]): Graph[V] =
    overlay(this, right)

  def *(right: Graph[V]): Graph[V] =
    connect(this, right)

  def ->(right: Graph[V]): Graph[V] =
    connect(this, right)
}

object Graph {

  protected[graph] final case class Empty[V]() extends Graph[V]
  protected[graph] final case class Vertex[V](v: V) extends Graph[V]
  protected[graph] final case class Overlay[V](left: Graph[V], right: Graph[V]) extends Graph[V]
  protected[graph] final case class Connect[V](left: Graph[V], right: Graph[V]) extends Graph[V]

  //Smart constructors -> ensure simplicity
  def empty[V]: Graph[V] = Empty()

  def vertex[V](v: V): Graph[V] = Vertex(v)

  def v[V](v: V): Graph[V] = vertex(v)

  def overlay[V](g1: Graph[V], g2: Graph[V]): Graph[V] = (g1,g2) match {
    case (Empty(), Empty()) => Empty()
    case (v@Vertex(_), Empty()) => v
    case (Empty(), v@Vertex(_)) => v
    case _ => Overlay(g1,g2)
  }

  def connect[V](g1: Graph[V], g2: Graph[V]): Graph[V] = (g1,g2) match {
    case (Empty(), Empty()) => Empty()
    case (v@Vertex(_), Empty()) => v
    case (Empty(), v@Vertex(_)) => v
    case _ => Connect(g1,g2)
  }

  def vertices[A](vs: Seq[A]): Graph[A] =
    vs.foldLeft[Graph[A]](empty)((g, v) => overlay(vertex(v), g))

  def edges[A](es: Seq[(A,A)]): Graph[A] =
    es.foldLeft[Graph[A]](empty)((g, e) => overlay(connect(vertex(e._1), vertex(e._2)), g))

  def overlays[A](gs: Seq[Graph[A]]): Graph[A] =
    gs.foldLeft[Graph[A]](empty)(overlay)

  def connects[A](gs: Seq[Graph[A]]): Graph[A] =
    gs.foldLeft[Graph[A]](empty)(connect)

  def create[A](vs: Seq[A], es: Seq[(A,A)]): Graph[A] =
    overlay(vertices(vs), edges(es))

  //Church encoded fold
  def fold[A,B](empty: B,
                vertex: A => B,
                overlay: (Graph[A], Graph[A])  => B,
                connect: (Graph[A], Graph[A]) => B)(graph: Graph[A]): B = graph match {
    case Empty() =>
      empty
    case Vertex(v) =>
      vertex(v)
    case Overlay(g1,g2) =>
      overlay(g1,g2)
    case Connect(g1,g2) =>
      connect(g1,g2)
  }

  //Generalized fold
  def foldg[A,B](empty: B,
                 vertex: A => B,
                 overlay: (B, B) => B,
                 connect: (B, B) => B)(graph: Graph[A]): B = graph match {
    case Empty() =>
      empty
    case Vertex(v) =>
      vertex(v)
    case Overlay(g1,g2) =>
      overlay(foldg(empty,vertex,overlay,connect)(g1),foldg(empty,vertex,overlay,connect)(g2))
    case Connect(g1,g2) =>
      connect(foldg(empty,vertex,overlay,connect)(g1),foldg(empty,vertex,overlay,connect)(g2))
  }

  //Stack-safe generalized fold
  def foldgEval[A,B](empty: B,
                     vertex: A => B,
                     overlay: (B, B) => B,
                     connect: (B, B) => B)(graph: Graph[A]): Eval[B] = graph match {
    case Empty() =>
      Eval.now(empty)
    case Vertex(v) =>
      Eval.now(vertex(v))
    case Overlay(go1,go2) =>
      val g1 = Eval.defer(foldgEval(empty,vertex,overlay,connect)(go1))
      val g2 = Eval.defer(foldgEval(empty,vertex,overlay,connect)(go2))
      (g1,g2).mapN(overlay)
    case Connect(go1,go2) =>
      val g1 = Eval.defer(foldgEval(empty, vertex, overlay, connect)(go1))
      val g2 = Eval.defer(foldgEval(empty, vertex, overlay, connect)(go2))
      (g1, g2).mapN(connect)
  }

  def map[A,B](graph: Graph[A])(f: A => B): Graph[B] = foldg[A, Graph[B]](
    empty,
    f.andThen(vertex[B]),
    overlay[B],
    connect[B])(graph)

  def flatMap[A,B](graph: Graph[A])(f: A => Graph[B]): Graph[B] = foldg[A, Graph[B]](
    empty,
    f,
    overlay[B],
    connect[B])(graph)

  def toList[A](graph: Graph[A]): List[A] = foldg[A, List[A]](
    Nil,
    x => List(x),
    _++_,
    _++_)(graph)

  def clique[A](vs: Seq[A]): Graph[A] =
    connects(vs.map(vertex))

  def star[A](v: A, vs: Seq[A]): Graph[A] =
    connect(vertex(v),vertices(vs))

  def path[A](vs: Seq[A]): Graph[A] = vs match {
    case Nil => empty
    case v :: Nil => vertex(v)
    case vs => edges(vs.zip(vs.tail))
  }

  def circuit[A](vs: Seq[A]): Graph[A] = vs match {
    case Nil => empty
    case v :: vs => path(List(v) ++ vs ++ List(v))
  }

  def box[A,B](g1: Graph[A], g2: Graph[B]): Graph[(A, B)] = {
    val xs = toList(g2).map(b => g1.map(a => (a, b)))
    val ys = toList(g1).map(a => g2.map(b => (a, b)))
    overlays(xs ++ ys)
  }

  def mesh[A,B](us: Seq[A], vs: Seq[B]): Graph[(A, B)] =
    box(path(us), path(vs))

  def torus[A,B](us: Seq[A], vs: Seq[B]): Graph[(A, B)] =
    box(circuit(us), circuit(vs))

  def vertexSet[A](graph: Graph[A]): Set[A] = foldg[A, Set[A]](
    Set.empty,
    Set(_),
    _++_,
    _++_)(graph)

  def edgeSet[A](graph: Graph[A]): Set[(A, A)] = graph match {
    case Empty() => Set.empty
    case Vertex(_) => Set.empty
    case Overlay(g1, g2) => edgeSet(g1) ++ edgeSet(g2)
    case Connect(g1, g2) => edgeSet(g1) ++ edgeSet(g2) ++ (for {
      v1 <- vertexSet(g1)
      v2 <- vertexSet(g2)
    } yield (v1,v2))
  }

  def transpose[A](graph: Graph[A]): Graph[A] = foldg[A, Graph[A]](
    empty,
    vertex,
    overlay,
    (g1,g2) => connect(g2,g1))(graph)

  def induce[A](f: A => Boolean)(graph: Graph[A]): Graph[A] = foldg[A, Graph[A]](
    empty,
    v => if (f(v)) vertex(v) else empty,
    overlay,
    connect)(graph)

  def removeVertex[A](v: A)(graph: Graph[A]): Graph[A] =
    induce[A](_ != v)(graph)

  def graphViz[A](title: String = "g")(graph: Graph[A]): String = {
    s"digraph $title {\n${edgeSet(graph).map(x => s""" "${x._1}" -> "${x._2}" """).mkString("\n")}\n}"
  }

  implicit val GraphIsMonad: Monad[Graph] = new Monad[Graph] {
    override def pure[A](x: A): Graph[A] =
      vertex(x)

    override def map[A, B](fa: Graph[A])(f: A => B): Graph[B] =
      Graph.map(fa)(f)

    override def flatMap[A, B](fa: Graph[A])(f: A => Graph[B]): Graph[B] =
      Graph.flatMap(fa)(f)

    private def flatMapSafe[A, B](fa: Graph[A])(f: A => Graph[B]): Eval[Graph[B]] = foldgEval[A, Graph[B]](
      empty,
      f,
      overlay[B],
      connect[B])(fa)

    override def tailRecM[A, B](a: A)(f: A => Graph[Either[A, B]]): Graph[B] = flatMapSafe(f(a)) {
      case Right(b) => pure(b)
      case Left(nextA) => tailRecM(nextA)(f)
    }.value
  }
}
