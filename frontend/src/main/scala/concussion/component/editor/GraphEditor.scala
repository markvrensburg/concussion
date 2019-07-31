package concussion
package component
package editor

import cats.effect.IO
import concussion.component.Logo
import concussion.nodes._
import concussion.styles.{GraphStyle, PageStyle}
import concussion.util.Namer
import japgolly.scalajs.react.CatsReact._
import concussion.util.CatsIOReact._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._

import scala.util.Random

object GraphEditor {

  sealed trait ConnectionState
  final case class Connecting(from: Port, to: Option[Anchor]) extends ConnectionState
  case object NotConnecting extends ConnectionState

  final case class Connection(port1: Port, port2: Port) {
    def containsId(portId: PortId): Boolean =
      (port1.id == portId) || (port2.id == portId)

    def connectsTo(portId: PortId): Option[Port] = this match {
      case Connection(Port(id, _), port) if id == portId => Some(port)
      case Connection(port, Port(id, _)) if id == portId => Some(port)
      case _                                             => None

    }
  }

  final case class State(
      connectionState: ConnectionState,
      offset: (Double, Double),
      logo: String,
      connections: Vector[Connection] = Vector.empty,
      nodes: Vector[(String, NodeType)] = Vector.empty
  )

  final case class Props(namer: Namer[IO])

  final class Backend($ : BackendScope[Props, State]) {

    private val editorRef = Ref[html.Element]

    private def findConnector(port: Port, connections: Vector[Connection]): Option[Port] =
      connections.find(_.containsId(port.id)).flatMap(_.connectsTo(port.id))

    private def filterConnector(port: Port, connections: Vector[Connection]): Vector[Connection] =
      connections.filter(!_.containsId(port.id))

    private def addNode(
        nodeType: NodeType
//        ports: Vector[(PortOrientation, String)] = Vector.empty,
//        code: Option[String] = Option.empty
    ): Callback =
      for {
        props <- $.props
        id <- props.namer.nextName(NodeType.nodeTypes.encode(nodeType)).toCallback
        _ <- $.modState(state => {
          state.copy(nodes = state.nodes :+ (id -> nodeType))
        })
      } yield ()

    private def deleteNode(nodeId: String): Callback =
      $.modState(state => {
        state.copy(nodes = state.nodes.filter(_._1 != nodeId))
      })

    private def bringToFront(nodeId: String): Callback =
      $.modState(state => {
        val split = state.nodes.span(_._1 != nodeId)
        state.copy(nodes = split._1 ++ split._2.tail :+ split._2.head)
      })

    private def adjustPorts(ports: Vector[Port]): Callback =
      $.modState(state => {
        val adjusted = state.connections.map {
          case c @ Connection(Port(id, _), port) if ports.exists(_.id == id) =>
            ports
              .find(_.id == id)
              .map(
                p =>
                  Connection(
                    Port(
                      id,
                      Anchor(
                        state.offset._1 + p.anchor.x,
                        state.offset._2 + p.anchor.y,
                        p.anchor.orientation
                      )
                    ),
                    port
                  )
              )
              .getOrElse(c)
          case c @ Connection(port, Port(id, _)) =>
            ports
              .find(_.id == id)
              .map(
                p =>
                  Connection(
                    port,
                    Port(
                      id,
                      Anchor(
                        state.offset._1 + p.anchor.x,
                        state.offset._2 + p.anchor.y,
                        p.anchor.orientation
                      )
                    )
                  )
              )
              .getOrElse(c)
          case c => c
        }
        state.copy(connections = adjusted)
      })

    private def deletePorts(ports: Vector[PortId]): Callback =
      $.modState(state => {
        val remaining =
          state.connections.filter(
            (c: Connection) => !ports.contains(c.port1.id) && !ports.contains(c.port2.id)
          )
        state.copy(connections = remaining)
      })

    private def onPortClick(port: Port): Callback =
      $.modState(state => {
        val portX = state.offset._1 + port.anchor.x
        val portY = state.offset._2 + port.anchor.y
        val currentPort = Port(port.id, Anchor(portX, portY, port.anchor.orientation))

        state.connectionState match {
          case Connecting(from, _) =>
            if (currentPort.id == from.id)
              state.copy(connectionState = NotConnecting)
            else if (currentPort.id.nodeId == from.id.nodeId)
              state
            else {
              findConnector(currentPort, state.connections) match {
                case Some(connectedPort) =>
                  val connection = Connection(from, currentPort)
                  val connections = filterConnector(currentPort, state.connections) :+ connection
                  state.copy(
                    connectionState = Connecting(connectedPort, Some(currentPort.anchor)),
                    connections = connections
                  )
                case None =>
                  val connection = Connection(from, currentPort)
                  val connections = state.connections :+ connection
                  state.copy(connectionState = NotConnecting, connections = connections)
              }
            }
          case NotConnecting =>
            findConnector(currentPort, state.connections) match {
              case Some(connectedPort) =>
                state.copy(
                  connectionState = Connecting(connectedPort, Some(currentPort.anchor)),
                  connections = filterConnector(currentPort, state.connections)
                )
              case None =>
                state.copy(
                  connectionState = Connecting(currentPort, None)
                )
            }
        }
      })

    private def onPortHover(port: Port): Callback =
      $.modState(state => {
        state.connectionState match {
          case Connecting(from, _) if port.id.nodeId == from.id.nodeId => state
          case Connecting(from, _) =>
            state.copy(
              connectionState = Connecting(
                from,
                Some(Anchor(port.anchor.x, port.anchor.y, port.anchor.orientation))
              )
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

    def render(props: Props, state: State): VdomElement =
      NodeMenu(
        state.logo,
        addNode,
        <.div(
          PageStyle.editor,
          <.div.withRef(editorRef)(
            GraphStyle.graphEditor,
            ^.id := GraphStyle.nodeEditorId,
            Infobar(),
            //Nodes
            React.Fragment(
              state.nodes.map(
                n =>
                  <.div(
                    ^.key := n._1,
                    Node(
                      n._1,
                      n._2,
                      props.namer,
                      onPortClick,
                      onPortHover,
                      adjustPorts,
                      deletePorts,
                      deleteNode(n._1),
                      bringToFront(n._1)
                    )
                  )
              ): _*
            ),
            //Connectors
            state.connections.toTagMod(
              c =>
                Connector(
                  Anchor(c.port1.anchor.x, c.port1.anchor.y, c.port1.anchor.orientation),
                  Anchor(c.port2.anchor.x, c.port2.anchor.y, c.port2.anchor.orientation)
                )
            ),
            state.connectionState match {
              case Connecting(from, to) =>
                InFlightConnector(from.anchor, to)
              case NotConnecting => EmptyVdom
            },
            //Event Listeners
            ^.onScroll --> updateOffset
          )
        )
      )
  }

  private def component(random: Random) =
    ScalaComponent
      .builder[Props]("GraphEditor")
      .initialState(State(NotConnecting, (0, 0), Logo(random)))
      .renderBackend[Backend]
      .build

  def apply(random: Random, namer: Namer[IO]): Unmounted[Props, State, Backend] =
    component(random)(Props(namer))
}
