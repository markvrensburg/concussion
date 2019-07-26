package concussion
package component
package editor

import concussion.geometry.{Point, Rect}
import concussion.styles.ConnectorStyle
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import scalatags.Text.svgAttrs._
import scalatags.Text.svgTags.{circle, _}
import scalatags.Text.implicits._
import scalacss.ScalaCssReact._

object Connector {

  val radius: Double = 6
  private val strokeWidth: Double = 2
  private val bezierWeight = 0.5

  private def clamp(min: Double, max: Double)(value: Double) =
    Math.max(min, Math.min(max, value))

  final case class Props(
      from: (Double, Double, PortOrientation),
      to: (Double, Double, PortOrientation),
      dashed: Boolean
  ) {

    val fromX = from._1
    val fromY = from._2
    val fromOrientation = from._3

    val toX = to._1
    val toY = to._2
    val toOrientation = to._3

    lazy val p0: Point = Point(fromX, fromY)

    lazy val p3: Point = Point(toX, toY)

    lazy val dx: Double = {
      val width = Math.abs(fromX - toX)
      val height = Math.abs(fromY - toY)
      if (width <= 50 && height <= 50 || height <= radius) 0
      else clamp(50, 250)(width * bezierWeight)
    }

    lazy val p1: Point = fromOrientation match {
      case Right => Point(p0.x + dx, p0.y)
      case Left  => Point(p0.x - dx, p0.y)
      case _     => Point(p0.x, p0.y)
    }

    lazy val p2: Point = toOrientation match {
      case Right => Point(p3.x + dx, p3.y)
      case Left  => Point(p3.x - dx, p3.y)
      case _     => Point(p3.x, p3.y)
    }

    lazy val boundingBox: Rect = {
      var mi = p0.min(p3)
      var ma = p0.max(p3)

      val c = p1 - p0 //-1.0 * p0 + 1.0 * p1;
      val b = p0 - (p1 * 2) + p2 //1.0*p0 - 2.0*p1 + 1.0*p2;
      val a = (p1 * 3) - p0 - (p2 * 3) + p3 //-1.0*p0 + 3.0*p1 - 3.0*p2 + 1.0*p3;

      var h = (b * b) - (a * c)

      @inline def q_(s: Double, t: Double, v1: Double, v2: Double, v3: Double, v4: Double) =
        (s * s * s * v1) + (3.0 * s * s * t * v2) + (3.0 * s * t * t * v3) + (t * t * t * v4)

      if (h.x > 0.0) {
        h = h.copy(x = Math.sqrt(h.x))
        var t = c.x / (-b.x - h.x)
        if (t > 0.0 && t < 1.0) {
          val s = 1.0 - t
          val q = q_(s, t, p0.x, p1.x, p2.x, p3.x)
          mi = mi.copy(x = Math.min(mi.x, q))
          ma = ma.copy(x = Math.max(ma.x, q))
        }
        t = c.x / (-b.x + h.x)
        if (t > 0.0 && t < 1.0) {
          val s = 1.0 - t
          val q = q_(s, t, p0.x, p1.x, p2.x, p3.x)
          mi = mi.copy(x = Math.min(mi.x, q))
          ma = ma.copy(x = Math.max(ma.x, q))
        }
      }
      if (h.y > 0.0) {
        h = h.copy(y = Math.sqrt(h.y))
        var t = c.x / (-b.y - h.y)
        if (t > 0.0 && t < 1.0) {
          val s = 1.0 - t
          val q = q_(s, t, p0.y, p1.y, p2.y, p3.y)
          mi = mi.copy(y = Math.min(mi.y, q))
          ma = ma.copy(y = Math.max(ma.y, q))
        }
        t = c.x / (-b.y + h.y)
        if (t > 0.0 && t < 1.0) {
          val s = 1.0 - t
          val q = q_(s, t, p0.y, p1.y, p2.y, p3.y)
          mi = mi.copy(y = Math.min(mi.y, q))
          ma = ma.copy(y = Math.max(ma.y, q))
        }
      }

      val box = Rect(mi, ma)

      val xLeft =
        if ((Math.min(p0.x, p3.x) - box.topLeft.x) <= radius)
          box.topLeft.x - radius
        else
          box.topLeft.x - strokeWidth

      val xRight =
        if ((box.bottomRight.x - Math.max(p0.x, p3.x)) <= radius)
          box.bottomRight.x + radius
        else
          box.bottomRight.x + strokeWidth

      val yTop = Math.min(mi.y, ma.y) - radius
      val yBot = Math.max(mi.y, ma.y) + radius

      Rect(Point(xLeft, yTop), Point(xRight, yBot))
    }

    def offset(point: Point) =
      Point(point.x - boundingBox.topLeft.x, point.y - boundingBox.topLeft.y)

    lazy val connector: String = {

      val p0Offset = offset(p0)
      val p1Offset = offset(p1)
      val p2Offset = offset(p2)
      val p3Offset = offset(p3)

      val x_1 = p0Offset.x
      val y_1 = p0Offset.y
      val x_2 = p3Offset.x
      val y_2 = p3Offset.y
      val c_1 = p1Offset.x
      val c_2 = p2Offset.x

      val connectorPath = s"M$x_1 $y_1 C $c_1 $y_1 $c_2 $y_2 $x_2 $y_2"

      val dashes = if (dashed) "stroke-dasharray: 4" else ""

      svg(
        style := "height: 100%; width: 100%",
        path(
          `class` := "connector",
          d := connectorPath,
          style := s"stroke-width: $strokeWidth; $dashes"
        ),
        circle(style := s"fill: white;", cx := x_1, cy := y_1, r := radius),
        circle(style := s"fill: white;", cx := x_2, cy := y_2, r := radius)
      ).render
    }
  }

  private val component =
    ScalaComponent
      .builder[Props]("Connector")
      .render_P(
        p =>
          <.div(
            ^.left := s"${p.boundingBox.topLeft.x}",
            ^.top := s"${p.boundingBox.topLeft.y}",
            ^.height := s"${p.boundingBox.height}",
            ^.width := s"${p.boundingBox.width}",
            ^.position := "absolute",
            ConnectorStyle.connector,
            ^.dangerouslySetInnerHtml := p.connector
          )
      )
      .build

  def apply(
      from: (Double, Double, PortOrientation),
      to: (Double, Double, PortOrientation),
      dashed: Boolean = false
  ): Unmounted[Props, Unit, Unit] =
    component(Props(from, to, dashed))
}
