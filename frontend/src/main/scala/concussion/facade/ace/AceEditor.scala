package concussion
package facade
package ace

import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Js.{RawMounted, UnmountedWithRawType}
import scalacss.ProdDefaults._

import scala.scalajs.js.annotation.JSImport
import scalajs.js

@js.native
trait AceEditor extends js.Object

object AceEditor {

  object Style extends StyleSheet.Inline {

    import dsl._

    style(unsafeRoot("#brace-editor")(
      borderRadius(0.5.em)
    ))
  }

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
    var mode: js.UndefOr[String]
    var theme: js.UndefOr[String]
    var keyboardHandler: js.UndefOr[String]
    var showGutter: js.UndefOr[Boolean]
    var wrapEnabled: js.UndefOr[Boolean]
  }

  def props(height: js.UndefOr[String] = js.undefined,
            width: js.UndefOr[String] = js.undefined,
            value: js.UndefOr[String] = js.undefined,
            onChange: js.UndefOr[OnChange] = js.undefined,
            mode: js.UndefOr[String] = js.undefined,
            theme: js.UndefOr[String] = js.undefined,
            keyboardHandler: js.UndefOr[String] = js.undefined,
            showGutter: js.UndefOr[Boolean] = js.undefined,
            wrapEnabled: js.UndefOr[Boolean] = js.undefined
           ): Props = {
    val p = (new js.Object).asInstanceOf[Props]
    p.height = height
    p.width = width
    p.value = value
    p.onChange = onChange.map(cb => (e: ReactEvent) => cb(e).runNow())
    p.mode = mode
    p.theme = theme
    p.keyboardHandler = keyboardHandler
    p.showGutter = showGutter
    p.wrapEnabled = wrapEnabled
    p
  }

  val component = JsComponent[Props, Children.None, Null](RawComponent)

  def apply(p: Props): UnmountedWithRawType[Props, Null, RawMounted[Props, Null]] =
   component.apply(p)
}