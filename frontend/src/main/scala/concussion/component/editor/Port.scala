package concussion.component.editor

sealed trait PortOrientation {
  def swap: PortOrientation = this match {
    case Right => Left
    case Left  => Right
    case None  => None
  }
}
case object Right extends PortOrientation
case object Left extends PortOrientation
case object None extends PortOrientation

final case class PortId(id: String, nodeId: String)

final case class Port(id: PortId, x: Double, y: Double, orientation: PortOrientation)
