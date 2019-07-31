package concussion
package component
package editor

import concussion.geometry.Point
import concussion.styles.GraphStyle
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.{EventListener, OnUnmount}
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import org.scalajs.dom.MouseEvent

object InFlightConnector {

  final case class Props(
      from: Anchor,
      to: Option[Anchor],
      offset: Point,
      fixed: Boolean,
      dashed: Boolean
  )

  final case class State(x: Double, y: Double)

  final class Backend($ : BackendScope[Props, State]) extends OnUnmount {

    def onMouseMove(e: MouseEvent): Callback =
      for {
        props <- $.props
        _ <- if (!props.fixed)
          $.modState(state => {
            state.copy(x = e.clientX, y = e.clientY)
          })
        else
          Callback.empty
      } yield ()

    def render(props: Props, state: State): VdomElement =
      Connector(
        Anchor(props.from.x, props.from.y, props.from.orientation),
        Anchor(
          state.x + props.offset.x,
          state.y + props.offset.y,
          props.to.map(_.orientation).getOrElse(Neutral)
        ),
        props.dashed
      )
  }

  private val component =
    ScalaComponent
      .builder[Props]("InFlightConnector")
      .initialStateFromProps(
        props =>
          props.to
            .map(anchor => State(anchor.x, anchor.y))
            .getOrElse(State(props.from.x, props.from.y))
      )
      .renderBackend[Backend]
      .configure(
        EventListener[MouseEvent].install(
          "mousemove",
          _.backend.onMouseMove,
          _ => dom.document.getElementById(GraphStyle.nodeEditorId)
        )
      )
      .shouldComponentUpdate(lc => CallbackTo(!lc.currentProps.fixed))
      .build

  def apply(
      from: Anchor,
      to: Option[Anchor] = Option.empty,
      offset: Point = Point(0, 0),
      fixed: Boolean = false,
      dashed: Boolean = true
  ): Unmounted[Props, State, Backend] =
    component(Props(from, to, offset, fixed, dashed))

}
