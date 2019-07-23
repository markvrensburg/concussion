package concussion
package component
package editor

import cats.effect.IO
import concussion.component.Logo
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
  final case class Connecting(from: Port, to: Port) extends ConnectionState
  case object NotConnecting extends ConnectionState

  final case class Connection(port1: Port, port2: Port)

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

    private def adjustPorts(ports: Vector[Port]): Callback =
      $.modState(state => {
        val adjusted = state.connections.map {
          case c @ Connection(Port(id, _, _, _), port) if ports.exists(_.id == id) =>
            ports
              .find(_.id == id)
              .map(
                p =>
                  Connection(
                    Port(id, state.offset._1 + p.x, state.offset._2 + p.y, p.orientation),
                    port
                  )
              )
              .getOrElse(c)
          case c @ Connection(port, Port(id, _, _, _)) =>
            ports
              .find(_.id == id)
              .map(
                p =>
                  Connection(
                    port,
                    Port(id, state.offset._1 + p.x, state.offset._2 + p.y, p.orientation)
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
        val portX = state.offset._1 + port.x
        val portY = state.offset._2 + port.y
        val currentPort = Port(port.id, portX, portY, port.orientation)

        state.connectionState match {
          case Connecting(from, _) => {
            if (currentPort.id == from.id)
              state.copy(connectionState = NotConnecting)
            else if (currentPort.id.nodeId == from.id.nodeId)
              state
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
              connectionState = Connecting(currentPort, currentPort.copy(orientation = Neutral))
            )
            //Check if connection already exists:
            //Yes - Delete existing and change to in flight
            //No - Start new inflight
          }
        }
      })

    private def onPortHover(port: Port): Callback =
      $.modState(state => {
        state.connectionState match {
          case Connecting(from, _) if port.id.nodeId == from.id.nodeId => state
          case Connecting(from, to) =>
            state.copy(
              connectionState = Connecting(from, to.copy(orientation = port.orientation))
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

    private def updateConnection(e: ReactMouseEvent): Callback = {
      val x = e.clientX
      val y = e.clientY
      $.modState(state => {
        state.connectionState match {
          case Connecting(from, to) =>
            state.copy(
              connectionState = Connecting(
                from,
                Port(PortId("", ""), state.offset._1 + x, state.offset._2 + y, to.orientation) //todo move connecting component out
              )
            )
          case NotConnecting => state
        }
      })
    }

    def render(props: Props, state: State): VdomElement =
      NodeMenu(
        state.logo,
        addNode,
        <.div(
          PageStyle.editor,
          <.div.withRef(editorRef)(
            GraphStyle.graphEditor,
            ^.id := "node-editor",
            Infobar(),
            //Toolbar(),
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
                      deleteNode(n._1)
                    )
                  )
              ): _*
            ),
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
      .builder[Props]("GraphEditor")
      .initialState(State(NotConnecting, (0, 0), Logo(random)))
      .renderBackend[Backend]
      .build

  def apply(random: Random, namer: Namer[IO]): Unmounted[Props, State, Backend] =
    component(random)(Props(namer))
}
