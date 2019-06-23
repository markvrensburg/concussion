package concussion
package component
package editor

import japgolly.scalajs.react._
import component.Scala.Unmounted
import vdom.html_<^._

object Input {

  final case class State(length: Int)

  final case class Props(defaultValue: String, onChange: String => Callback)

  final class Backend($ : BackendScope[Props, State]) {

    private def onChange(next: String => Callback)(e: ReactEventFromInput) = {
      val newValue = e.target.value
      $.modState(_.copy(length = newValue.length)) >> next(newValue)
    }

    def render(props: Props, state: State): VdomElement =
      <.div(
        ^.cls := "ui inverted transparent input",
        <.input(
          ^.`type` := "text",
          ^.fontFamily := "monospace",
          ^.size := state.length,
          ^.defaultValue := props.defaultValue,
          ^.onChange ==> onChange(props.onChange)
        )
      )
  }

  private def component(initialLength: Int) =
    ScalaComponent
      .builder[Props]("Input")
      .initialState(State(initialLength))
      .renderBackend[Backend]
      .build

  def apply(
      defaultValue: String = "",
      initialLength: Int = 20,
      onChange: String => Callback = _ => Callback(())
  ): Unmounted[Props, State, Backend] =
    component(if (defaultValue.isEmpty) initialLength else defaultValue.length)(
      Props(defaultValue, onChange)
    )

}
