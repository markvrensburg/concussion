package concussion.component.editor

import concussion.styles.ConnectorStyle
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import scalatags.Text.svgAttrs._
import scalatags.Text.svgTags._
import scalatags.Text.implicits._
import scalacss.ScalaCssReact._

object Connector {

  val radius: Double = 6
  private val strokeWidth: Double = 4
  private val bezierWeight = 0.7

  final case class Props(from: Port, to: Port) {

    private val orderPortX = Vector(from, to).sortBy(_.x)
    private val orderPortY = Vector(from, to).sortBy(_.y)

    val left: Port = orderPortX.head
    val right: Port = orderPortX(1)

    val top: Port = orderPortY.head
    val bottom: Port = orderPortY(1)

    val height: Double = bottom.y - top.y
    val width: Double = right.x - left.x

    def connector: String = {

      val x_1 = radius
      val x_2 = radius + width

      val (y_1, y_2) = (left.y, right.y) match {
        case (topY, botY) if topY < botY => (radius, radius + height)
        case _                           => (radius + height, radius)
      }

      val dx = width * bezierWeight
      val c1 = from.orientation match {
        case Right => x_1 + dx
        case Left  => x_1 - dx
        case None  => x_1
      }

      val c2 = to.orientation match {
        case Right => x_2 + dx
        case Left  => x_2 - dx
        case None  => x_2
      }

      val connectorPath = s"M$x_1 $y_1 C $c1 $y_1 $c2 $y_2 $x_2 $y_2"

      svg(
        style := "height: 100%; width: 100%",
        path(
          `class` := "connector",
          d := connectorPath,
          style := s"fill:none; stroke: white; stroke-opacity: 0.6; stroke-width: $strokeWidth;"
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
            ^.left := s"${p.left.x - radius}",
            ^.top := s"${p.top.y - radius}",
            ^.height := s"${p.height + (2 * radius)}",
            ^.width := s"${p.width + (2 * radius)}", //todo take into account bezier curve width
            ^.position := "absolute",
            ConnectorStyle.connector,
            ^.dangerouslySetInnerHtml := p.connector
          )
      )
      .build

  def apply(from: Port, to: Port): Unmounted[Props, Unit, Unit] =
    component(Props(from, to))
}
