package concussion
package component
package editor

sealed trait PortOrientation {
  def swap: PortOrientation = this match {
    case Right   => Left
    case Left    => Right
    case Neutral => Neutral
  }
}
case object Right extends PortOrientation
case object Left extends PortOrientation
case object Neutral extends PortOrientation

final case class PortId(id: String, nodeId: String)

final case class Port(id: PortId, x: Double, y: Double, orientation: PortOrientation)
