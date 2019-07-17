package concussion.component.editor

import concussion.facade.ace.{AceEditor, EditorProps}
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._

object CodeEditor {

  final case class State(code: String)

  final case class Props(initialCode: String, maxLines: Int)

  final class Backend($ : BackendScope[Props, State]) {

    private val updateCode: AceEditor.OnChange =
      (s: String) => $.modState(_.copy(code = s))

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
            onChange = updateCode,
            minLines = state.code.lines.size,
            maxLines = props.maxLines,
            wrapEnabled = true,
            debounceChangePeriod = 100,
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
      .shouldComponentUpdate(lc => CallbackTo(lc.currentState.code != lc.nextState.code))
      .build

  def apply(initialCode: String = "", maxLines: Int = 12): Unmounted[Props, State, Backend] =
    component(Props(initialCode, maxLines))

}
