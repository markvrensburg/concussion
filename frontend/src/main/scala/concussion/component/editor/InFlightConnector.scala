package concussion
package component
package editor

import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._

object InFlightConnector {

  final case class Props(from: Port, to: Option[Port], fixed: Boolean, dashed: Boolean)

  final case class State(length: Int)

  final class Backend() {

    def render(props: Props): VdomElement =
      Connector(props.from, props.to.getOrElse(props.from), props.dashed)
  }

  private val component =
    ScalaComponent
      .builder[Props]("InFlightConnector")
      .initialState(State(1))
      .renderBackend[Backend]
      .build

  def apply(
      from: Port,
      to: Option[Port] = Option.empty,
      fixed: Boolean = false,
      dashed: Boolean = true
  ): Unmounted[Props, State, Backend] =
    component(Props(from, to, fixed, dashed))

}
