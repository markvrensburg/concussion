package concussion
package component

import concussion.styles.ConnectorStyle
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import scalatags.Text.svgAttrs._
import scalatags.Text.svgTags._
import scalatags.Text.implicits._
import scalacss.ScalaCssReact._

object Connector {

  case class Props(x1: Int, y1: Int, x2: Int, y2: Int) {
    private val bezierWeight = 0.7

    def connector = {

      val dx = Math.abs(x1 - x2) * bezierWeight

      val xs = Math.min(x1,x2)
      val xe = Math.max(x1,x2)

      val c1 = xs + dx
      val c2 = xe - dx

      val connectorPath = s"M$xs $y1 C $c1 $y1 $c2 $y2 $xe $y2"

      svg(style := s"height: 100%; width: 100%; position: absolute",
        path(
          d := connectorPath,
          style := s"fill:none; stroke: white; stroke-opacity: 0.6; stroke-width: 4;",
        ),
        circle(`class` := "connect-handle", style := s"fill: white;", cx := x1, cy := y1, r := 6),
        circle(`class` := "connect-handle", style := s"fill: white;", cx := x2, cy := y2, r := 6),
      ).render
    }
  }

  private val component = ScalaComponent.builder[Props]("Connector")
      .render_P(p => <.div(ConnectorStyle.connector, ^.dangerouslySetInnerHtml := p.connector))
      .build

  def apply(x1: Int, y1: Int, x2: Int, y2: Int): Unmounted[Props, Unit, Unit] =
    component.apply(Props(x1,y1,x2,y2))
}
