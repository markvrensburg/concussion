package concussion
package geometry

final case class Point(x: Double, y: Double) {

  def add(point: Point): Point =
    Point(x + point.x, y + point.y)

  def +(point: Point): Point =
    add(point)

  def sub(point: Point): Point =
    Point(x - point.x, y - point.y)

  def -(point: Point): Point =
    sub(point)

  def mul(scalar: Double): Point =
    Point(scalar * x, scalar * y)

  def mul(point: Point): Point =
    Point(x * point.x, y * point.y)

  def *(scalar: Double): Point =
    mul(scalar)

  def *(point: Point): Point =
    mul(point)

  def maxByX(point: Point): Point =
    if (x > point.x) this else point

  def maxByY(point: Point): Point =
    if (y > point.y) this else point

  def minByX(point: Point): Point =
    if (x <= point.x) this else point

  def minByY(point: Point): Point =
    if (y <= point.y) this else point

  def max(point: Point): Point =
    Point(maxByX(point).x, maxByY(point).y)

  def min(point: Point): Point =
    Point(minByX(point).x, minByY(point).y)

}
