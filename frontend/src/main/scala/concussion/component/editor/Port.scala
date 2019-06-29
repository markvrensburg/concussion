package concussion.component.editor

sealed trait PortOrientation
case object Right extends PortOrientation
case object Left extends PortOrientation
case object None extends PortOrientation

final case class Port(x: Double, y: Double, orientation: PortOrientation)
