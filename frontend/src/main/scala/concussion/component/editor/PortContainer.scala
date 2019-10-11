package concussion
package component
package editor

import concussion.domain.Port
import concussion.geometry._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.colors.Grey
import react.semanticui.elements.icon.Icon

object PortContainer {

  final case class Props(port: EditPort,
                         canDelete: Boolean,
                         onPortClick: EditPort => Callback,
                         onPortHover: EditPort => Callback,
                         onDeleteClick: Callback,
                         onShiftClick: Callback,
                         onNameChange: String => Callback)

  final class Backend($ : BackendScope[Props, Unit]) {

    private def onPortEvent(
      callback: EditPort => Callback,
      orientation: Option[Orientation] = Option.empty
    ): Callback =
      $.props.flatMap(
        props =>
          callback(
            Port(
              props.port.id,
              props.port.name,
              (orientation.getOrElse(props.port.meta._1), props.port.meta._2)
            )
        )
      )

    private def portSocket(props: Props): VdomNode =
      <.div.withRef(props.port.meta._2)(
        ^.onMouseUp --> onPortEvent(props.onPortClick),
        ^.onMouseEnter --> onPortEvent(props.onPortHover),
        ^.onMouseLeave --> onPortEvent(props.onPortHover, Some(Neutral)),
        Icon(Icon.props(name = "dot circle outline", className = "port-socket"))
      )

    private def delete(props: Props): Option[VdomNode] =
      if (props.canDelete)
        Some(
          <.div(
            ^.onClick --> props.onDeleteClick,
            Icon(
              Icon.props(
                name = "trash alternate outline",
                color = Grey,
                link = true
              )
            )
          )
        )
      else
        Option.empty

    private def shift(props: Props): VdomNode =
      <.div(
        ^.onClick --> props.onShiftClick,
        Icon(
          Icon.props(
            name =
              if (props.port.meta._1 == Right) "angle left" else "angle right",
            color = Grey,
            link = true
          )
        )
      )

    private def elements(props: Props): Vector[VdomNode] =
      Vector(
        portSocket(props),
        Name(defaultValue = props.port.name, onChange = props.onNameChange),
        delete(props),
        shift(props)
      )

    def render(props: Props): VdomElement =
      <.div(
        ^.width := "100%",
        ^.display := "flex",
        ^.justifyContent := {
          props.port.meta._1 match {
            case Right => "flex-end"
            case _     => "flex-start"
          }
        },
        props.port.meta._1 match {
          case Right =>
            React.Fragment(elements(props).reverse: _*)
          case _ =>
            React.Fragment(elements(props): _*)
        }
      )
  }

  private val component =
    ScalaComponent
      .builder[Props]("PortContainer")
      .renderBackend[Backend]
      .shouldComponentUpdate(
        lc =>
          CallbackTo(lc.currentProps.port.meta._1 != lc.nextProps.port.meta._1)
      )
      .build

  def apply(
    port: EditPort,
    canDelete: Boolean = true,
    onPortClick: EditPort => Callback = _ => Callback.empty,
    onPortHover: EditPort => Callback = _ => Callback.empty,
    onDeleteClick: Callback = Callback.empty,
    onShiftClick: Callback = Callback.empty,
    onNameChange: String => Callback = _ => Callback.empty
  ): Unmounted[Props, Unit, Backend] =
    component(
      Props(
        port,
        canDelete,
        onPortClick,
        onPortHover,
        onDeleteClick,
        onShiftClick,
        onNameChange
      )
    )
}
