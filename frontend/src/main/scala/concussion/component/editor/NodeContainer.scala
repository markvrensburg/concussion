package concussion
package component
package editor

import cats.implicits._
import cats.effect.IO
import concussion.compile.Validation
import concussion.domain.Node._
import concussion.facade.draggable.{
  Draggable,
  DraggableBounds,
  DraggableData,
  Grid
}
import concussion.domain._
import concussion.geometry._
import concussion.styles.NodeStyle
import concussion.util.{Namer, Nodes, Ports}
import japgolly.scalajs.react._
import japgolly.scalajs.react.CatsReact._
import concussion.util.CatsIOReact._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.MouseEvent
import react.semanticui.colors.{Blue, Green, Grey, Red}
import react.semanticui.elements.header.Header
import react.semanticui.elements.icon.Icon
import react.semanticui.elements.segment.{Segment, SegmentAttached}
import react.semanticui.textalignment.Center
import scalacss.ScalaCssReact._

object NodeContainer {

  final case class State(doUpdate: Boolean = false)

  final case class Props(node: EditNode,
                         ports: Set[EditPort],
                         namer: Namer[IO],
                         onPortClick: EditPort => Callback,
                         onPortHover: EditPort => Callback,
                         adjustPorts: List[EditPort] => Callback,
                         deletePorts: List[String] => Callback,
                         addVertices: (EditNode, List[EditPort]) => Callback,
                         cloneNode: Callback,
                         deleteNode: Callback,
                         bringToFront: Callback)

  final class Backend($ : BackendScope[Props, State]) {

    private val getPorts =
      for {
        props <- $.props
        ports <- props.ports
          .map(port => {
            port.meta._2.get
              .map(e => {
                val rect = e.getBoundingClientRect
                val center = (
                  rect.left + ((rect.right - rect.left) / 2),
                  rect.top + ((rect.bottom - rect.top) / 2)
                )
                EditPort(
                  PortMeta(
                    port.meta._1.id,
                    Anchor(
                      center._1,
                      center._2,
                      port.meta._1.anchor.orientation
                    )
                  ),
                  port.meta._2,
                  port.name
                )
              })
              .asCallback
          })
          .toList
          .sequence
      } yield ports

    def updateConnections(force: Boolean = false): Callback =
      for {
        props <- $.props
        state <- $.state
        ports <- getPorts
        _ <- if (force || state.doUpdate)
          props.adjustPorts(ports.mapFilter(identity)) >> $.setState(
            State(doUpdate = true)
          )
        else
          Callback.empty
      } yield ()

    private val addPort =
      for {
        props <- $.props
        port <- Ports.mkPort(props.node, props.namer).toCallback
        _ <- props.addVertices(props.node, List(port))
      } yield ()

    private def deletePort(portId: String) =
      for {
        props <- $.props
        _ <- $.setState(State(doUpdate = true))
        _ <- props.deletePorts(List(portId))
      } yield ()

    private val onDelete: Callback =
      for {
        props <- $.props
        _ <- props.deletePorts(props.ports.toList.map(_.meta._1.id))
        _ <- props.deleteNode
      } yield ()

    private val onClone: Callback =
      for {
        props <- $.props
        _ <- props.cloneNode
      } yield ()

    private def shiftPort(portId: String) =
      for {
        props <- $.props
        ports = props.ports.map {
          case p @ Port(meta, _) if meta._1.id == portId =>
            p.map(
              m =>
                (
                  m._1.copy(
                    anchor = meta._1.anchor
                      .copy(orientation = meta._1.anchor.orientation.swap)
                  ),
                  m._2
              )
            )
          case port => port
        }
        _ <- props.adjustPorts(ports.toList)
        _ <- $.setState(State(doUpdate = true))
      } yield ()

    private def changePortName(portId: String)(newName: String) =
      for {
        props <- $.props
        ports = props.ports.map {
          case p @ Port(meta, _) if meta._1.id == portId =>
            p.copy(name = newName)
          case port => port
        }
        _ <- props.adjustPorts(ports.toList)
        _ <- $.setState(State(doUpdate = true))
      } yield ()

    private def onCodeChange(newCode: String) =
      for {
        props <- $.props
        _ <- $.modState { state =>
          println(
            s"${props.node.meta.id}: ${Validation.program[Long](newCode)}"
          )
          state.copy(doUpdate = true)
        }
      } yield ()
    //$.modState(_.copy(code = Some(newCode), doUpdate = true))

    private val bounds = DraggableBounds(-169, null, -20, null)

    private val nodeOptions =
      <.div(
        ^.width := "100%",
        ^.display := "flex",
        ^.justifyContent := "center",
        <.div(
          ^.onClick --> onDelete,
          Icon(
            Icon.props(
              name = "trash alternate outline",
              color = Grey,
              link = true
            )
          )
        ),
        <.div(
          ^.onClick --> onClone,
          Icon(Icon.props(name = "clone outline", color = Grey, link = true))
        )
      )

    private def input(props: Props) =
      Draggable(
        Draggable
          .props(
            grid = Grid(5, 5),
            handle = ".dragger",
            bounds = bounds,
            onMouseDown = (_: MouseEvent) => props.bringToFront,
            onStop = (_: MouseEvent, _: DraggableData) =>
              updateConnections(force = true)
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
              props.ports.toList.map(
                p =>
                  <.div(
                    ^.key := p.meta._1.id,
                    PortContainer(
                      p.meta._1.id,
                      p.name,
                      p.meta._1.anchor.orientation,
                      p.meta._2,
                      canDelete = false,
                      props.onPortClick,
                      props.onPortHover,
                      deletePort(p.meta._1.id),
                      shiftPort(p.meta._1.id),
                      changePortName(p.meta._1.id)
                    )
                )
              ): _*
            )
          )
        )
      )

    private def output(props: Props) =
      Draggable(
        Draggable
          .props(
            grid = Grid(5, 5),
            handle = ".dragger",
            bounds = bounds,
            onMouseDown = (_: MouseEvent) => props.bringToFront,
            onStop = (_: MouseEvent, _: DraggableData) =>
              updateConnections(force = true)
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
              props.ports.toList.map(
                p =>
                  <.div(
                    ^.key := p.meta._1.id,
                    PortContainer(
                      p.meta._1.id,
                      p.name,
                      p.meta._1.anchor.orientation,
                      p.meta._2,
                      canDelete = false,
                      props.onPortClick,
                      props.onPortHover,
                      deletePort(p.meta._1.id),
                      shiftPort(p.meta._1.id),
                      changePortName(p.meta._1.id)
                    )
                )
              ): _*
            )
          )
        )
      )

    private def processor(props: Props) =
      Draggable(
        Draggable
          .props(
            grid = Grid(5, 5),
            handle = ".dragger",
            bounds = bounds,
            onMouseDown = (_: MouseEvent) => props.bringToFront,
            onStop = (_: MouseEvent, _: DraggableData) =>
              updateConnections(force = true)
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
            Segment.props(
              inverted = true,
              compact = true,
              attached = SegmentAttached.Attached
            ),
            CodeEditor(onChange = onCodeChange)
          ),
          Segment(
            Segment.props(
              inverted = true,
              compact = true,
              attached = SegmentAttached.Bottom
            ),
            //Ports
            React.Fragment(
              props.ports.toList.map(
                p =>
                  <.div(
                    ^.key := p.meta._1.id,
                    PortContainer(
                      p.meta._1.id,
                      p.name,
                      p.meta._1.anchor.orientation,
                      p.meta._2,
                      canDelete = true,
                      props.onPortClick,
                      props.onPortHover,
                      deletePort(p.meta._1.id),
                      shiftPort(p.meta._1.id),
                      changePortName(p.meta._1.id)
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

    def render(props: Props): VdomElement = props.node match {
      case InputNode(_)        => input(props)
      case OutputNode(_)       => output(props)
      case ProcessorNode(_, _) => processor(props)
    }
  }

  private val component =
    ScalaComponent
      .builder[Props]("NodeContainer")
      .initialState(State())
      .renderBackend[Backend]
      .shouldComponentUpdate(
        lc =>
          CallbackTo {
            val currentProps = lc.currentProps
            val nextProps = lc.nextProps

            Nodes.shouldUpdateNode(currentProps.node, nextProps.node) ||
            Ports.shouldUpdatePorts(currentProps.ports, nextProps.ports)
        }
      )
      .componentDidUpdate(_.backend.updateConnections())
      .build

  def apply(
    node: EditNode,
    ports: Set[EditPort],
    namer: Namer[IO],
    onPortClick: EditPort => Callback = _ => Callback.empty,
    onPortHover: EditPort => Callback = _ => Callback.empty,
    adjustPorts: List[EditPort] => Callback = _ => Callback.empty,
    deletePorts: List[String] => Callback = _ => Callback.empty,
    addVertices: (EditNode, List[EditPort]) => Callback = (_, _) =>
      Callback.empty,
    cloneNode: Callback = Callback.empty,
    deleteNode: Callback = Callback.empty,
    bringToFront: Callback = Callback.empty
  ): Unmounted[Props, State, Backend] =
    component(
      Props(
        node,
        ports,
        namer,
        onPortClick,
        onPortHover,
        adjustPorts,
        deletePorts,
        addVertices,
        cloneNode,
        deleteNode,
        bringToFront
      )
    )
}
