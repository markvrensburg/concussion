package concussion
package component
package editor

import japgolly.scalajs.react._
import component.Scala.Unmounted
import vdom.html_<^._

object Name {

  final case class State(value: String)

  final case class Props(defaultValue: String, onChange: String => Callback)

  final class Backend($ : BackendScope[Props, State]) {

    private def onChange(next: String => Callback)(e: ReactEventFromInput) = {
      val newValue = e.target.value.replaceAll(" ", "_")
      $.modState(_.copy(value = newValue)) >> next(newValue)
    }

    def render(props: Props, state: State): VdomElement =
      <.div(
        ^.cls := "ui inverted transparent input",
        <.input(
          ^.`type` := "text",
          ^.fontFamily := "monospace",
          ^.size := { if (state.value.length <= 0) 5 else state.value.length },
          ^.value := state.value,
          ^.onChange ==> onChange(props.onChange)
        )
      )
  }

  private val component =
    ScalaComponent
      .builder[Props]("Name")
      .initialStateFromProps(p => State(p.defaultValue))
      .renderBackend[Backend]
      .build

  def apply(
      defaultValue: String = "",
      onChange: String => Callback = _ => Callback.empty
  ): Unmounted[Props, State, Backend] =
    component(Props(defaultValue, onChange))

}
