package concussion.component.editor

import japgolly.scalajs.react.Ref.Simple
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import react.semanticui.colors.Grey
import react.semanticui.elements.icon.Icon

object PortContainer {

  final case class State(doUpdate: Boolean)

  final case class Props(
      id: PortId,
      name: String,
      orientation: PortOrientation,
      portSocketRef: Simple[html.Element],
      canDelete: Boolean,
      onPortClick: Port => Callback,
      onPortHover: PortOrientation => Callback,
      onDeleteClick: Callback,
      onShiftClick: Callback,
      updateConnections: Callback = Callback.empty
  )

  final class Backend($ : BackendScope[Props, State]) {

    //todo look at better alternatives than using an "doUpdate" flag
    val updateConnections: Callback =
      for {
        props <- $.props
        state <- $.state
        _ <- if (state.doUpdate) props.updateConnections >> $.setState(State(doUpdate = false))
        else Callback.empty
      } yield ()

    private val onShiftClick: Callback =
      for {
        props <- $.props
        _ <- $.setState(State(doUpdate = true)) >> props.onShiftClick
      } yield ()

    private val onPortClick =
      for {
        props <- $.props
        _ <- props.portSocketRef.foreachCB(e => {
          val rect = e.getBoundingClientRect
          val center =
            (rect.left + ((rect.right - rect.left) / 2), rect.top + ((rect.bottom - rect.top) / 2))
          props.onPortClick(Port(props.id, center._1, center._2, props.orientation))
        })
      } yield ()

    private def portSocket(props: Props) =
      <.div.withRef(props.portSocketRef)(
        ^.onMouseUp --> onPortClick,
        ^.onMouseEnter --> props.onPortHover(props.orientation),
        ^.onMouseLeave --> props.onPortHover(None),
        Icon(Icon.props(name = "dot circle outline", className = "port-socket"))
      )

    private def delete(props: Props) =
      <.div(
        ^.onClick --> props.onDeleteClick,
        Icon(
          Icon.props(name = "trash alternate outline", color = Grey, link = true)
        )
      )

    private def shift(props: Props) =
      <.div(
        ^.onClick --> onShiftClick,
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
              Name(defaultValue = props.name),
              portSocket(props)
            )
          case _ =>
            React.Fragment(
              portSocket(props),
              Name(defaultValue = props.name),
              delete(props),
              shift(props)
            )
        }
      )
  }

  private val component =
    ScalaComponent
      .builder[Props]("PortContainer")
      .initialState(State(doUpdate = false))
      .renderBackend[Backend]
      .componentDidUpdate(_.backend.updateConnections)
      .build

  def apply(
      id: PortId,
      name: String,
      orientation: PortOrientation,
      portSocketRef: Simple[html.Element],
      canDelete: Boolean = true,
      onPortClick: Port => Callback = _ => Callback.empty,
      onPortHover: PortOrientation => Callback = _ => Callback.empty,
      onDeleteClick: Callback = Callback.empty,
      onShiftClick: Callback = Callback.empty,
      updateConnections: Callback = Callback.empty
  ): Unmounted[Props, State, Backend] =
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
        updateConnections
      )
    )

}
