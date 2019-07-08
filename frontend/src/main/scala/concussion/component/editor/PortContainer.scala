package concussion.component.editor

import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import react.semanticui.colors.Grey
import react.semanticui.elements.icon.Icon

object PortContainer {

  final case class Props(
      name: String,
      orientation: PortOrientation,
      onPortClick: Port => Callback,
      onPortHover: PortOrientation => Callback
  )

  final class Backend() {

    private val portSocketRef = Ref[html.Element]

    private def onPortClick(props: Props) =
      portSocketRef.foreachCB(e => {
        val rect = e.getBoundingClientRect
        val center =
          (rect.left + ((rect.right - rect.left) / 2), rect.top + ((rect.bottom - rect.top) / 2))
        props.onPortClick(Port(center._1, center._2, props.orientation))
      })

    private def portSocket(props: Props) =
      <.div.withRef(portSocketRef)(
        ^.onMouseUp --> onPortClick(props),
        ^.onMouseEnter --> props.onPortHover(props.orientation),
        ^.onMouseLeave --> props.onPortHover(None),
        Icon(Icon.props(name = "dot circle outline", className = "port-socket"))
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
            Icon(
              Icon.props(name = "angle left", color = Grey, link = true)
            ),
            Icon(
              Icon.props(name = "trash alternate outline", color = Grey, link = true)
            ),
            Name(defaultValue = props.name),
            portSocket(props)
          )
        case _ =>
          React.Fragment(
            portSocket(props),
            Name(defaultValue = props.name),
            Icon(
              Icon.props(name = "trash alternate outline", color = Grey, link = true)
            ),
            Icon(
              Icon.props(name = "angle right", color = Grey, link = true)
            )
          )
      }
    )
  }

  private val component =
    ScalaComponent
      .builder[Props]("PortContainer")
      .renderBackend[Backend]
      .build

  def apply(
      name: String,
      orientation: PortOrientation,
      onPortClick: Port => Callback = _ => Callback.empty,
      onPortHover: PortOrientation => Callback = _ => Callback.empty
  ): Unmounted[Props, Unit, Backend] =
    component(Props(name, orientation, onPortClick, onPortHover))

}
