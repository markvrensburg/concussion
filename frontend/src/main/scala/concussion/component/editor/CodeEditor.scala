package concussion
package component
package editor

import concussion.facade.ace.{AceEditor, EditorProps}
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import scala.concurrent.duration._
import scala.scalajs.js

object CodeEditor {

  final case class State(code: String)

  final case class Props(initialCode: String,
                         maxLines: Int,
                         onChange: String => Callback)

  final class Backend($ : BackendScope[Props, State]) {

    private val updateCode: AceEditor.OnChange =
      (newCode: String) =>
        for {
          props <- $.props
          _ <- $.modState(_.copy(code = newCode))
          //js.timer is a hacky solution to wait for refs to update.
          _ <- CallbackTo(
            js.timers
              .setTimeout(25.milliseconds)(props.onChange(newCode).runNow())
          )
        } yield ()

    def render(props: Props, state: State): VdomElement =
      <.div(
        ^.height := "100%",
        ^.minWidth := "210px",
        AceEditor(
          AceEditor.props(
            width = "100%",
            mode = "yaml",
            theme = "merbivore",
            value = state.code,
            placeholder = "Type code here",
            onChange = updateCode,
            minLines = state.code.lines.size,
            maxLines = props.maxLines,
            wrapEnabled = true,
            editorProps = EditorProps(blockScrolling = true)
          )
        )
      )
  }

  private val component =
    ScalaComponent
      .builder[Props]("CodeEditor")
      .initialStateFromProps(props => State(props.initialCode))
      .renderBackend[Backend]
      .shouldComponentUpdate(
        lc => CallbackTo(lc.currentState.code != lc.nextState.code)
      )
      .build

  def apply(
    initialCode: String = "",
    maxLines: Int = 12,
    onChange: String => Callback = _ => Callback.empty
  ): Unmounted[Props, State, Backend] =
    component(Props(initialCode, maxLines, onChange))

}
