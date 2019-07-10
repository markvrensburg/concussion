package concussion.component.editor

import cats.effect.IO
import concussion.component.Logo
import concussion.styles.{GraphStyle, PageStyle}
import concussion.util.Namer
import concussion.util.CatsReact._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._

import scala.util.Random

object GraphEditor {

  sealed trait ConnectionState
  final case class Connecting(from: Port, to: Port) extends ConnectionState
  case object NotConnecting extends ConnectionState

  final case class Connection(port1: Port, port2: Port)

  final case class State(
      connectionState: ConnectionState,
      offset: (Double, Double),
      logo: String,
      showMenu: Boolean = true,
      connections: Vector[Connection] = Vector.empty,
      nodes: Vector[(String, NodeType)] = Vector.empty
  )

  final case class Props(namer: Namer[IO])

  final class Backend($ : BackendScope[Props, State]) {

    private val editorRef = Ref[html.Element]

    private def addNode(nodeType: NodeType): Callback =
//      editorRef.foreachCB { e =>
//        val rect = e.getBoundingClientRect
//        val center = (rect.width / 2, rect.height / 2)
      for {
        props <- $.props
        id <- props.namer.nextName(NodeType.nodeTypes.encode(nodeType)).toCallback
        _ <- $.modState(state => {
          state.copy(nodes = state.nodes :+ (id -> nodeType))
        })
      } yield ()
//      }

//    private def adjustPorts(ports: Set[Port]): Callback =
//      $.modState(state => {})

    private def onPortClick(port: Port): Callback =
      $.modState(state => {
        val portX = state.offset._1 + port.x
        val portY = state.offset._2 + port.y
        val currentPort = Port(portX, portY, port.orientation)

        state.connectionState match {
          case Connecting(from, _) => {
            if (currentPort == from)
              state.copy(connectionState = NotConnecting)
            else {
              val connection = Connection(from, currentPort)
              val connections = state.connections :+ connection
              state.copy(connectionState = NotConnecting, connections = connections)
            }

            //Check if "from" - cancel in flight
            //Check if in same node - do nothing ? how node id...
            //Check if existing connection
            //Yes -
            //No - commit connection
          }
          case NotConnecting => {

            state.copy(
              connectionState = Connecting(currentPort, currentPort.copy(orientation = None))
            )
            //Check if connection already exists:
            //Yes - Delete existing and change to in flight
            //No - Start new inflight
          }
        }
      })

    private def onPortHover(orientation: PortOrientation): Callback =
      $.modState(state => {
        state.connectionState match {
          case Connecting(from, to) =>
            state.copy(
              connectionState = Connecting(from, to.copy(orientation = orientation))
            )
          case NotConnecting => state
        }
      })

    private val updateOffset =
      editorRef.foreachCB(editor => {
        val xOffset = editor.scrollLeft
        val yOffset = editor.scrollTop
        $.modState(_.copy(offset = (xOffset, yOffset)))
      })

    private val toggleMenu =
      $.modState(
        state =>
          state.showMenu match {
            case true  => state.copy(showMenu = false)
            case false => state.copy(showMenu = true)
          }
      )

    private def updateConnection(e: ReactMouseEvent): Callback = {
      val x = e.clientX
      val y = e.clientY
      $.modState(state => {
        state.connectionState match {
          case Connecting(from, to) =>
            state.copy(
              connectionState = Connecting(
                from,
                Port(state.offset._1 + x, state.offset._2 + y, to.orientation)
              )
            )
          case NotConnecting => state
        }
      })
    }

    def render(props: Props, state: State): VdomElement =
      NodeMenu(
        state.logo,
        state.showMenu,
        addNode,
        toggleMenu,
        <.div(
          PageStyle.editor,
          <.div.withRef(editorRef)(
            GraphStyle.graphEditor,
            ^.id := "node-editor",
            Infobar(),
            //Toolbar(),
            //Nodes
            state.nodes.toTagMod(n => Node(n._1, n._2, props.namer, onPortClick, onPortHover)),
            //Connectors
            state.connections.toTagMod(c => Connector(c.port1, c.port2)),
            state.connectionState match {
              case Connecting(from, to) => Connector(from, to, dashed = true)
              case NotConnecting        => EmptyVdom
            },
            //Event Listeners
            ^.onScroll --> updateOffset,
            (state.connectionState match {
              case Connecting(_, _) => Option[TagMod](^.onMouseMove ==> updateConnection)
              case NotConnecting    => Option.empty[TagMod]
            }).whenDefined
          )
        )
      )
  }

  private def component(random: Random) =
    ScalaComponent
      .builder[Props]("NodeEditor")
      .initialState(State(NotConnecting, (0, 0), Logo(random)))
      .renderBackend[Backend]
      .build

  def apply(random: Random, namer: Namer[IO]): Unmounted[Props, State, Backend] =
    component(random)(Props(namer))
}
