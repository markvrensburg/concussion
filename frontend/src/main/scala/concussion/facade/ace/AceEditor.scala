package concussion
package facade
package ace

import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Js.{RawMounted, UnmountedWithRawType}
import japgolly.scalajs.react.raw.JsNumber

import scala.scalajs.js.annotation.JSImport
import scalajs.js

@js.native
trait AceEditor extends js.Object

object AceEditor {

  @js.native
  @JSImport("react-ace", JSImport.Default)
  object RawComponent extends js.Object

  type OnChange = ReactEvent => Callback

  @js.native
  trait Props extends js.Object {
    var height: js.UndefOr[String]
    var width: js.UndefOr[String]
    var value: js.UndefOr[String]
    var onChange: js.UndefOr[raw.RawOnChange]
    var minLines: js.UndefOr[JsNumber]
    var maxLines: js.UndefOr[JsNumber]
    var debounceChangePeriod: js.UndefOr[JsNumber]
    var mode: js.UndefOr[String]
    var theme: js.UndefOr[String]
    var readOnly: js.UndefOr[Boolean]
    var keyboardHandler: js.UndefOr[String]
    var showGutter: js.UndefOr[Boolean]
    var wrapEnabled: js.UndefOr[Boolean]
    var editorProps: js.UndefOr[EditorProps]
  }

  def props(
      height: js.UndefOr[String] = js.undefined,
      width: js.UndefOr[String] = js.undefined,
      value: js.UndefOr[String] = js.undefined,
      onChange: js.UndefOr[OnChange] = js.undefined,
      minLines: js.UndefOr[JsNumber] = js.undefined,
      maxLines: js.UndefOr[JsNumber] = js.undefined,
      debounceChangePeriod: js.UndefOr[JsNumber] = js.undefined,
      mode: js.UndefOr[String] = js.undefined,
      theme: js.UndefOr[String] = js.undefined,
      readOnly: js.UndefOr[Boolean] = js.undefined,
      keyboardHandler: js.UndefOr[String] = js.undefined,
      showGutter: js.UndefOr[Boolean] = js.undefined,
      wrapEnabled: js.UndefOr[Boolean] = js.undefined,
      editorProps: js.UndefOr[EditorProps] = js.undefined
  ): Props = {
    val p = (new js.Object).asInstanceOf[Props]
    p.height = height
    p.width = width
    p.value = value
    p.onChange = onChange.map(cb => (e: ReactEvent) => cb(e).runNow())
    p.minLines = minLines
    p.maxLines = maxLines
    p.debounceChangePeriod = debounceChangePeriod
    p.mode = mode
    p.theme = theme
    p.readOnly = readOnly
    p.keyboardHandler = keyboardHandler
    p.showGutter = showGutter
    p.wrapEnabled = wrapEnabled
    p.editorProps = editorProps
    p
  }

  private val component = JsComponent[Props, Children.None, Null](RawComponent)

  def apply(p: Props): UnmountedWithRawType[Props, Null, RawMounted[Props, Null]] =
    component.apply(p)
}
