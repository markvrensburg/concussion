package concussion
package domain

import cats.data.Chain

final case class Port[Meta](id: String, name: String, meta: Meta) {
  def mapMeta[B](f: Meta => B): Port[B] =
    this.copy(meta = f(meta))
}

sealed trait NodePorts[Meta] {
  def toChain: Chain[Port[Meta]] = this match {
    case Single(port)    => Chain(port)
    case Multiple(ports) => ports
  }

  def toList: List[Port[Meta]] = this match {
    case Single(port)    => List(port)
    case Multiple(ports) => ports.toList
  }

  def map[B](f: Port[Meta] => Port[B]): NodePorts[B] = this match {
    case Single(port)    => Single(f(port))
    case Multiple(ports) => Multiple(ports.map(f))
  }
}

final case class Single[Meta](port: Port[Meta]) extends NodePorts[Meta]
final case class Multiple[Meta](ports: Chain[Port[Meta]])
    extends NodePorts[Meta]
