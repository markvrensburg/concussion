package concussion.geometry

sealed trait Orientation {
  def swap: Orientation = this match {
    case Right   => Left
    case Left    => Right
    case Neutral => Neutral
  }
}
case object Right extends Orientation
case object Left extends Orientation
case object Neutral extends Orientation
