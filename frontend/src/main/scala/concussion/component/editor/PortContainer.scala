package concussion.component.editor

import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.colors.Grey
import react.semanticui.elements.icon.Icon
import react.semanticui.modules.popup.{Popup, PopupContent, PopupOn, PopupPosition}

object PortContainer {

  final case class Props(name: String, orientation: PortOrientation)

  final class Backend() {

    private val portOptions: VdomNode =
      Icon(Icon.props(name = "setting", className = "port-options", color = Grey, link = true))

    private val portOptionsContent: VdomNode =
      <.div("Port Options")

    private def portOptionsPopup(popupPosition: PopupPosition): VdomNode =
      Popup(
        Popup.props(
          trigger = portOptions,
          position = popupPosition,
          content = PopupContent.props(content = portOptionsContent),
          on = PopupOn.Click,
          hideOnScroll = true
        )
      )

    def render(props: Props): VdomElement = <.div(
      ^.width := "100%",
      ^.display := "flex",
      ^.justifyContent := {
        props.orientation match {
          case Right => "flex-end"
          case _     => "flex-start"
        }
      },
      props.orientation match {
        case Right =>
          React.Fragment(
            portOptionsPopup(PopupPosition.LeftCenter),
            Input(defaultValue = props.name),
            PortSocket()
          )
        case _ =>
          React.Fragment(
            PortSocket(),
            Input(defaultValue = props.name),
            portOptionsPopup(PopupPosition.RightCenter)
          )
      }
    )
  }

  private val component =
    ScalaComponent
      .builder[Props]("PortContainer")
      .renderBackend[Backend]
      .build

  def apply(name: String, orientation: PortOrientation): Unmounted[Props, Unit, Backend] =
    component(Props(name, orientation))

}
