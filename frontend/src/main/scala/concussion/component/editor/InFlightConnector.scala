package concussion
package component
package editor

import concussion.styles.GraphStyle
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.{EventListener, OnUnmount}
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import org.scalajs.dom.MouseEvent

object InFlightConnector {

  final case class Props(from: Port, to: Option[Port], dashed: Boolean)

  final case class State(x: Double, y: Double, orientation: PortOrientation)

  final class Backend($ : BackendScope[Props, State]) extends OnUnmount {

    def onMouseMove(e: MouseEvent): Callback =
      $.modState(state => {
        state.copy(x = e.clientX, y = e.clientY)
      })

    def render(props: Props, state: State): VdomElement =
      Connector(
        (props.from.x, props.from.y, props.from.orientation),
        (state.x, state.y, state.orientation),
        props.dashed
      )
  }

  private val component =
    ScalaComponent
      .builder[Props]("InFlightConnector")
      .initialStateFromProps(
        props =>
          props.to
            .map(port => State(port.x, port.y, port.orientation))
            .getOrElse(State(props.from.x, props.from.y, Neutral))
      )
      .renderBackend[Backend]
      .configure(
        EventListener[MouseEvent].install(
          "mousemove",
          _.backend.onMouseMove,
          _ => dom.document.getElementById(GraphStyle.nodeEditorId)
        )
      )
      .build

  def apply(
      from: Port,
      to: Option[Port] = Option.empty,
      dashed: Boolean = true
  ): Unmounted[Props, State, Backend] =
    component(Props(from, to, dashed))

}
