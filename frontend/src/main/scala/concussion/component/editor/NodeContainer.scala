package concussion
package component
package editor

import cats.effect.IO
import concussion.compile.Validation
import concussion.domain.Node._
import concussion.facade.draggable._
import concussion.domain._
import concussion.styles.NodeStyle
import concussion.util.{Namer, Nodes, Ports}
import japgolly.scalajs.react._
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
                         namer: Namer[IO],
                         onPortClick: EditPort => Callback,
                         onPortHover: EditPort => Callback,
                         newPort: Callback,
                         modifyPorts: (EditPort => EditPort) => Callback,
                         updateCode: String => Callback,
                         deletePort: String => Callback,
                         cloneNode: Callback,
                         deleteNode: Callback,
                         bringToFront: Callback)

  final class Backend($ : BackendScope[Props, State]) {

    def updateConnections(force: Boolean = false): Callback =
      Callback(println(force))
//      for {
//        props <- $.props
//        state <- $.state
//        _ <- if (force || state.doUpdate)
//          props.adjustPorts(ports.mapFilter(identity)) >> $.setState(
//            State(doUpdate = true)
//          )
//        else
//          Callback.empty
//      } yield ()

    private val onDelete: Callback =
      for {
        props <- $.props
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
        modify = (_: EditPort) match {
          case port @ Port(id, _, meta) if id == portId =>
            port.mapMeta(m => (meta._1.swap, m._2))
          case port => port
        }
        _ <- props.modifyPorts(modify)
        _ <- $.setState(State(doUpdate = true))
      } yield ()

    private def changePortName(portId: String)(newName: String) =
      for {
        props <- $.props
        modify = (_: EditPort) match {
          case port @ Port(id, _, _) if id == portId =>
            port.copy(name = newName)
          case port => port
        }
        _ <- props.modifyPorts(modify)
        _ <- $.setState(State(doUpdate = true))
      } yield ()

    private def deletePort(portId: String) =
      for {
        props <- $.props
        _ <- $.setState(State(doUpdate = true))
        _ <- props.deletePort(portId)
      } yield ()

    private def onCodeChange(newCode: String) =
      for {
        props <- $.props
        _ <- props.updateCode(newCode)
        _ <- $.modState { state =>
          println(s"${props.node.id}: ${Validation.program[Long](newCode)}")
          state.copy(doUpdate = true)
        }
      } yield ()

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
        ),
        <.div(
          //^.onClick --> settings,
          Icon(
            Icon.props(name = "sliders horizontal", color = Grey, link = true)
          )
        )
      )

    private def header(node: Node[_, _]): VdomNode = node match {
      case InputNode(_, _, _) =>
        Header(Header.props(as = "h4", inverted = true, color = Green), "INPUT")
      case OutputNode(_, _, _) =>
        Header(Header.props(as = "h4", inverted = true, color = Red), "OUTPUT")
      case ProcessorNode(_, _, _, _) =>
        Header(
          Header.props(as = "h4", inverted = true, color = Blue),
          "PROCESSOR"
        )
    }

    private def codeEditor(node: Node[_, _]): Option[VdomNode] = node match {
      case InputNode(_, _, _)  => None
      case OutputNode(_, _, _) => None
      case ProcessorNode(_, code, _, _) =>
        Some(
          Segment(
            Segment.props(
              inverted = true,
              compact = true,
              attached = SegmentAttached.Attached
            ),
            CodeEditor(initialCode = code, onChange = onCodeChange)
          )
        )
    }

    private def ports(props: Props): VdomNode = {
      val canDelete = props.node.ports match {
        case Single(_)   => false
        case Multiple(_) => true
      }
      React.Fragment(
        props.node.ports.toList.map(
          port =>
            <.div(
              ^.key := port.id,
              PortContainer(
                port,
                canDelete,
                props.onPortClick,
                props.onPortHover,
                deletePort(port.id),
                shiftPort(port.id),
                changePortName(port.id)
              )
          )
        ): _*
      )
    }

    private def newPortButton(props: Props): Option[VdomNode] =
      props.node.ports match {
        case Single(_) => None
        case Multiple(_) =>
          Some(
            <.div(
              ^.width := "100%",
              ^.display := "flex",
              ^.justifyContent := "center",
              ^.marginTop := ".25rem",
              <.div(
                ^.onClick --> props.newPort,
                Icon(Icon.props(name = "plus circle", link = true))
              )
            )
          )
      }

    def render(props: Props) =
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
        <.div.withRef(props.node.meta)(
          NodeStyle.nodePos,
          Segment(
            Segment.props(
              className = "dragger",
              inverted = true,
              compact = true,
              attached = SegmentAttached.Top,
              textAlign = Center
            ),
            header(props.node),
            nodeOptions
          ),
          codeEditor(props.node),
          Segment(
            Segment.props(
              inverted = true,
              compact = true,
              attached = SegmentAttached.Bottom,
              textAlign = Center
            ),
            //Ports
            ports(props),
            newPortButton(props)
          )
        )
      )
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
            Ports
              .shouldUpdatePorts(currentProps.node.ports, nextProps.node.ports)
        }
      )
      .componentDidUpdate(_.backend.updateConnections())
      .build

  def apply(
    node: EditNode,
    namer: Namer[IO],
    onPortClick: EditPort => Callback = _ => Callback.empty,
    onPortHover: EditPort => Callback = _ => Callback.empty,
    newPort: Callback = Callback.empty,
    modifyPorts: (EditPort => EditPort) => Callback = _ => Callback.empty,
    updateCode: String => Callback = _ => Callback.empty,
    deletePort: String => Callback = _ => Callback.empty,
    cloneNode: Callback = Callback.empty,
    deleteNode: Callback = Callback.empty,
    bringToFront: Callback = Callback.empty
  ): Unmounted[Props, State, Backend] =
    component(
      Props(
        node,
        namer,
        onPortClick,
        onPortHover,
        newPort,
        modifyPorts,
        updateCode,
        deletePort,
        cloneNode,
        deleteNode,
        bringToFront
      )
    )
}
