package concussion.component.editor

import cats.implicits._
import cats.effect.IO
import concussion.facade.draggable.{Draggable, DraggableBounds, DraggableData, Grid}
import concussion.styles.NodeStyle
import concussion.util.Namer
import enum.Enum
import japgolly.scalajs.react.Ref.Simple
import japgolly.scalajs.react._
import japgolly.scalajs.react.CatsReact._
import concussion.util.CatsIOReact._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.{MouseEvent, html}
import react.semanticui.colors.{Blue, Green, Grey, Red}
import react.semanticui.elements.header.Header
import react.semanticui.elements.icon.Icon
import react.semanticui.elements.segment.{Segment, SegmentAttached}
import react.semanticui.textalignment.Center
import scalacss.ScalaCssReact._

sealed trait NodeType
case object Input extends NodeType
case object Output extends NodeType
case object Processor extends NodeType

object NodeType {
  val nodeTypes: Enum[NodeType] = Enum.derived[NodeType]
}

object Node {

  final case class State(
      ports: Vector[(String, Simple[html.Element], PortOrientation, String)] = Vector.empty,
      code: Option[String] = Option.empty,
      doUpdate: Boolean = false
  )

  final case class Props(
      id: String,
      nodeType: NodeType,
      namer: Namer[IO],
      onPortClick: Port => Callback,
      onPortHover: PortOrientation => Callback,
      adjustPorts: Vector[Port] => Callback,
      deletePorts: Vector[PortId] => Callback,
      deleteNode: Callback
  )

  final class Backend($ : BackendScope[Props, State]) {

    private val getPorts =
      for {
        props <- $.props
        state <- $.state
        ports <- state.ports
          .map(p => {
            p._2.get
              .map(e => {
                val rect = e.getBoundingClientRect
                val center = (
                  rect.left + ((rect.right - rect.left) / 2),
                  rect.top + ((rect.bottom - rect.top) / 2)
                )
                Port(PortId(p._1, props.id), center._1, center._2, p._3)
              })
              .asCallback
          })
          .sequence
      } yield ports

    def updateConnections(force: Boolean = false): Callback =
      for {
        props <- $.props
        state <- $.state
        ports <- getPorts
        _ <- if (force || state.doUpdate)
          props.adjustPorts(ports.filter(_.isDefined).map(_.get)) >> $.modState(
            _.copy(doUpdate = false)
          )
        else
          Callback.empty
      } yield ()

    private val addPort =
      for {
        props <- $.props
        id <- props.namer.nextName(s"${props.id}_Port").toCallback
        _ <- $.modState(state => {
          state.copy(
            ports = state.ports :+ (
              (
                id,
                Ref[html.Element],
                if (props.nodeType == Input) Right
                else Left,
                "Port"
              )
            )
          )
        })
      } yield ()

    private def deletePort(portId: PortId) =
      for {
        props <- $.props
        _ <- $.modState(state => {
          state.copy(ports = state.ports.filter(_._1 != portId.id), doUpdate = true) //todo make id's type safe
        })
        _ <- props.deletePorts(Vector(portId))
      } yield ()

    private val onDelete: Callback =
      for {
        props <- $.props
        state <- $.state
        _ <- props.deletePorts(state.ports.map(p => PortId(p._1, props.id)))
        _ <- props.deleteNode
      } yield ()

    private def shiftPort(portId: PortId) =
      $.modState(state => {
        val ports = state.ports.map {
          case (id, ref, orientation, name) if id == portId.id =>
            (id, ref, orientation.swap, name)
          case port => port
        }
        state.copy(ports = ports, doUpdate = true)
      })

    private def changePortName(portId: PortId)(newName: String) =
      $.modState(state => {
        val ports = state.ports.map {
          case (id, ref, orientation, _) if id == portId.id =>
            (id, ref, orientation, newName)
          case port => port
        }
        state.copy(ports = ports, doUpdate = true)
      })

    private def onCodeChange(newCode: String) =
      $.modState(_.copy(code = Some(newCode), doUpdate = true))

    private val bounds = DraggableBounds(-199, null, 0, null)

    private val nodeOptions =
      <.div(
        ^.width := "100%",
        ^.display := "flex",
        ^.justifyContent := "center",
        <.div(
          ^.onClick --> onDelete,
          Icon(
            Icon.props(name = "trash alternate outline", color = Grey, link = true)
          )
        ),
        <.div(
          //^.onClick --> ???,
          Icon(
            Icon.props(name = "clone outline", color = Grey, link = true)
          )
        )
      )

    private def input(props: Props, state: State) =
      Draggable(
        Draggable
          .props(
            grid = Grid(5, 5),
            handle = ".dragger",
            bounds = bounds,
            //onDrag = (_: MouseEvent, _: DraggableData) => updateConnections,
            onStart = (_: MouseEvent, _: DraggableData) => Callback(println("Starting")),
            onStop = (_: MouseEvent, _: DraggableData) => updateConnections(force = true)
          ),
        <.div(
          //          ^.left := "50%",
          //          ^.top := "50%",
          //          ^.transform := "translate(-50%,-50%)",
          NodeStyle.nodePos,
          Segment(
            Segment.props(
              className = "dragger",
              inverted = true,
              compact = true,
              attached = SegmentAttached.Top,
              textAlign = Center
            ),
            Header(
              Header.props(as = "h4", inverted = true, color = Green),
              "INPUT"
            ),
            nodeOptions
          ),
          Segment(
            Segment.props(
              inverted = true,
              compact = true,
              attached = SegmentAttached.Bottom,
              textAlign = Center
            ),
            //Ports
            React.Fragment(
              state.ports.map(
                p =>
                  <.div(
                    ^.key := p._1,
                    PortContainer(
                      PortId(p._1, props.id),
                      p._4,
                      p._3,
                      p._2,
                      canDelete = false,
                      props.onPortClick,
                      props.onPortHover,
                      deletePort(PortId(p._1, props.id)),
                      shiftPort(PortId(p._1, props.id)),
                      changePortName(PortId(p._1, props.id))
                    )
                  )
              ): _*
            )
          )
        )
      )

    private def output(props: Props, state: State) =
      Draggable(
        Draggable
          .props(
            grid = Grid(5, 5),
            handle = ".dragger",
            bounds = bounds,
            //onDrag = (_: MouseEvent, _: DraggableData) => updateConnections,
            onStop = (_: MouseEvent, _: DraggableData) => updateConnections(force = true)
          ),
        <.div(
          NodeStyle.nodePos,
          Segment(
            Segment.props(
              className = "dragger",
              inverted = true,
              compact = true,
              attached = SegmentAttached.Top,
              textAlign = Center
            ),
            Header(
              Header.props(as = "h4", inverted = true, color = Red),
              "OUTPUT"
            ),
            nodeOptions
          ),
          Segment(
            Segment.props(
              inverted = true,
              compact = true,
              attached = SegmentAttached.Bottom,
              textAlign = Center
            ),
            //Ports
            React.Fragment(
              state.ports.map(
                p =>
                  <.div(
                    ^.key := p._1,
                    PortContainer(
                      PortId(p._1, props.id),
                      p._4,
                      p._3,
                      p._2,
                      canDelete = false,
                      props.onPortClick,
                      props.onPortHover,
                      deletePort(PortId(p._1, props.id)),
                      shiftPort(PortId(p._1, props.id)),
                      changePortName(PortId(p._1, props.id))
                    )
                  )
              ): _*
            )
          )
        )
      )

    private def processor(props: Props, state: State) =
      Draggable(
        Draggable
          .props(
            grid = Grid(5, 5),
            handle = ".dragger",
            bounds = bounds,
            //onDrag = (_: MouseEvent, _: DraggableData) => updateConnections,
            onStop = (_: MouseEvent, _: DraggableData) => updateConnections(force = true)
          ),
        <.div(
          NodeStyle.nodePos,
          Segment(
            Segment.props(
              className = "dragger",
              inverted = true,
              compact = true,
              attached = SegmentAttached.Top,
              textAlign = Center
            ),
            Header(
              Header.props(as = "h4", inverted = true, color = Blue),
              "PROCESSOR"
            ),
            nodeOptions
          ),
          Segment(
            Segment.props(inverted = true, compact = true, attached = SegmentAttached.Attached),
            CodeEditor(onChange = onCodeChange)
          ),
          Segment(
            Segment.props(inverted = true, compact = true, attached = SegmentAttached.Bottom),
            //Ports
            React.Fragment(
              state.ports.map(
                p =>
                  <.div(
                    ^.key := p._1,
                    PortContainer(
                      PortId(p._1, props.id),
                      p._4,
                      p._3,
                      p._2,
                      canDelete = true,
                      props.onPortClick,
                      props.onPortHover,
                      deletePort(PortId(p._1, props.id)),
                      shiftPort(PortId(p._1, props.id)),
                      changePortName(PortId(p._1, props.id))
                    )
                  )
              ): _*
            ),
            <.div(
              ^.width := "100%",
              ^.display := "flex",
              ^.justifyContent := "center",
              ^.marginTop := ".25rem",
              <.div(
                ^.onClick --> addPort,
                Icon(Icon.props(name = "plus circle", link = true))
              )
            )
          )
        )
      )

    def render(props: Props, state: State): VdomElement = props.nodeType match {
      case Input     => input(props, state)
      case Output    => output(props, state)
      case Processor => processor(props, state)
    }
  }

  private val component =
    ScalaComponent
      .builder[Props]("Node")
      .initialStateCallbackFromProps(
        props =>
          props.namer
            .nextName(s"${props.id}_Port")
            .toCallback
            .map(
              name =>
                State(
                  ports = Vector(
                    (
                      name,
                      Ref[html.Element],
                      if (props.nodeType == Input) Right
                      else Left,
                      "Port"
                    )
                  )
                )
            )
      )
      .renderBackend[Backend]
      .componentDidUpdate(_.backend.updateConnections())
      .build

  def apply(
      id: String,
      nodeType: NodeType,
      namer: Namer[IO],
      onPortClick: Port => Callback = _ => Callback.empty,
      onPortHover: PortOrientation => Callback = _ => Callback.empty,
      adjustPorts: Vector[Port] => Callback = _ => Callback.empty,
      deletePorts: Vector[PortId] => Callback = _ => Callback.empty,
      deleteNode: Callback = Callback.empty
  ): Unmounted[Props, State, Backend] =
    component(
      Props(id, nodeType, namer, onPortClick, onPortHover, adjustPorts, deletePorts, deleteNode)
    )
}
