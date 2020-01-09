package concussion
package component
package editor

import concussion.domain.PortMeta
import concussion.geometry._
import japgolly.scalajs.react.Ref.Simple
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import react.semanticui.colors.Grey
import react.semanticui.elements.icon.Icon

object PortContainer {

  final case class Props(
      id: String,
      name: String,
      orientation: Orientation,
      portSocketRef: Simple[html.Element],
      canDelete: Boolean,
      onPortClick: EditPort => Callback,
      onPortHover: EditPort => Callback,
      onDeleteClick: Callback,
      onShiftClick: Callback,
      onNameChange: String => Callback
  )

  final class Backend($ : BackendScope[Props, Unit]) {

    private def onPortEvent(
        callback: EditPort => Callback,
        orientation: Option[Orientation] = Option.empty
    ): Callback =
      for {
        props <- $.props
        _ <- props.portSocketRef.foreachCB(e => {
          val rect = e.getBoundingClientRect
          val center =
            (
              rect.left + ((rect.right - rect.left) / 2),
              rect.top + ((rect.bottom - rect.top) / 2)
            )
          callback(
            EditPort(
              PortMeta(
                props.id,
                Anchor(
                  center._1,
                  center._2,
                  orientation.getOrElse(props.orientation)
                )
              ),
              props.portSocketRef,
              props.name
            )
          )
        })
      } yield ()

    private def portSocket(props: Props): VdomNode =
      <.div.withRef(props.portSocketRef)(
        ^.onMouseUp --> onPortEvent(props.onPortClick),
        ^.onMouseEnter --> onPortEvent(props.onPortHover),
        ^.onMouseLeave --> onPortEvent(props.onPortHover, Some(Neutral)),
        Icon(name = "dot circle outline", className = "port-socket")
      )

    private def delete(props: Props): Option[VdomNode] =
      if (props.canDelete)
        Some(
          <.div(
            ^.onClick --> props.onDeleteClick,
            Icon(
              name = "trash alternate outline",
              color = Grey,
              link = true
            )
          )
        )
      else
        Option.empty

    private def shift(props: Props): VdomNode =
      <.div(
        ^.onClick --> props.onShiftClick,
        Icon(
          name = if (props.orientation == Right) "angle left" else "angle right",
          color = Grey,
          link = true
        )
      )

    private def elements(props: Props): Vector[VdomNode] =
      Vector(
        portSocket(props),
        Name(defaultValue = props.name, onChange = props.onNameChange),
        delete(props),
        shift(props)
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
        lc => CallbackTo(lc.currentProps.orientation != lc.nextProps.orientation)
      )
      .build

  def apply(
      id: String,
      name: String,
      orientation: Orientation,
      portSocketRef: Simple[html.Element],
      canDelete: Boolean = true,
      onPortClick: EditPort => Callback = _ => Callback.empty,
      onPortHover: EditPort => Callback = _ => Callback.empty,
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
