package concussion

import concussion.styles.LayoutStyle
import concussion.util.Events._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.{EventListener, OnUnmount}
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom._
import org.scalajs.dom.raw.Event
import scalacss.ScalaCssReact._

object Layout {

  final case class State(background: String)

  final case class Props(updateBackground: CallbackTo[String])

  final class Backend($ : BackendScope[Props, State]) extends OnUnmount {

    val onBackgroundChange: Event => Callback = _ =>
      for {
        props <- $.props
        background <- props.updateBackground
        _ <- $.setState(State(background))
      } yield ()

    def render(state: State, children: PropsChildren): VdomElement =
      React.Fragment(
        <.div(
          LayoutStyle.layout,
          ^.id := LayoutStyle.layoutId,
          ^.background := state.background
        ),
        children
      )
  }

  private val component =
    ScalaComponent
      .builder[Props]("Layout")
      .initialStateCallbackFromProps(_.updateBackground.map(State))
      .renderBackendWithChildren[Backend]
      .configure(
        EventListener[Event].install(
          backgroundChangeEvent,
          _.backend.onBackgroundChange,
          _ => document.getElementById(LayoutStyle.layoutId)
        )
      )
      .build

  def apply(
    updateBackground: CallbackTo[String]
  )(children: VdomNode): Unmounted[Props, State, Backend] =
    component(Props(updateBackground))(children)

}
