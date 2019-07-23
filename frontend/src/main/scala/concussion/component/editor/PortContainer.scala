package concussion
package component
package editor

import japgolly.scalajs.react.Ref.Simple
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import react.semanticui.colors.Grey
import react.semanticui.elements.icon.Icon

object PortContainer {

  final case class Props(
      id: PortId,
      name: String,
      orientation: PortOrientation,
      portSocketRef: Simple[html.Element],
      canDelete: Boolean,
      onPortClick: Port => Callback,
      onPortHover: Port => Callback,
      onDeleteClick: Callback,
      onShiftClick: Callback,
      onNameChange: String => Callback
  )

  final class Backend($ : BackendScope[Props, Unit]) {

    private def onPortEvent(
        callback: Port => Callback,
        orientation: Option[PortOrientation] = Option.empty
    ): Callback =
      for {
        props <- $.props
        _ <- props.portSocketRef.foreachCB(e => {
          val rect = e.getBoundingClientRect
          val center =
            (rect.left + ((rect.right - rect.left) / 2), rect.top + ((rect.bottom - rect.top) / 2))
          callback(Port(props.id, center._1, center._2, orientation.getOrElse(props.orientation)))
        })
      } yield ()

    private def portSocket(props: Props): VdomNode =
      <.div.withRef(props.portSocketRef)(
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
              Icon.props(name = "trash alternate outline", color = Grey, link = true)
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
            name = if (props.orientation == Right) "angle left" else "angle right",
            color = Grey,
            link = true
          )
        )
      )

    def render(props: Props): VdomElement =
      <.div(
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
              shift(props),
              delete(props),
              Name(defaultValue = props.name, onChange = props.onNameChange),
              portSocket(props)
            )
          case _ =>
            React.Fragment(
              portSocket(props),
              Name(defaultValue = props.name, onChange = props.onNameChange),
              delete(props),
              shift(props)
            )
        }
      )
  }

  private val component =
    ScalaComponent
      .builder[Props]("PortContainer")
      .renderBackend[Backend]
      .shouldComponentUpdate(
        lc => CallbackTo(lc.currentProps.orientation != lc.nextProps.orientation)
      )
      .build

  def apply(
      id: PortId,
      name: String,
      orientation: PortOrientation,
      portSocketRef: Simple[html.Element],
      canDelete: Boolean = true,
      onPortClick: Port => Callback = _ => Callback.empty,
      onPortHover: Port => Callback = _ => Callback.empty,
      onDeleteClick: Callback = Callback.empty,
      onShiftClick: Callback = Callback.empty,
      onNameChange: String => Callback = _ => Callback.empty
  ): Unmounted[Props, Unit, Backend] =
    component(
      Props(
        id,
        name,
        orientation,
        portSocketRef,
        canDelete,
        onPortClick,
        onPortHover,
        onDeleteClick,
        onShiftClick,
        onNameChange
      )
    )

}
