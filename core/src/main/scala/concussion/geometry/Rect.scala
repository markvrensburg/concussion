package concussion
package geometry

final case class Rect(topLeft: Point, bottomRight: Point) {

  lazy val height: Double = bottomRight.y - topLeft.y

  lazy val width: Double = bottomRight.x - topLeft.x
}
