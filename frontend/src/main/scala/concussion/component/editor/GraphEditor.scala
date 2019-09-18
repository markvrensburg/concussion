package concussion
package component
package editor

import cats.effect.IO
import concussion.geometry.{Anchor, Point}
import concussion.domain._
import concussion.graph.Graph
import concussion.styles.{GraphStyle, PageStyle}
import concussion.util.{Namer, Nodes}
import japgolly.scalajs.react.CatsReact._
import concussion.util.CatsIOReact._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._

object GraphEditor {

  sealed trait ConnectionState
  final case class Connecting(from: EditPort, to: Option[Anchor])
      extends ConnectionState
  case object NotConnecting extends ConnectionState

  final case class State(connectionState: ConnectionState,
                         offset: Point,
                         connections: Vector[EditConnection] = Vector.empty,
                         nodes: Vector[EditNode] = Vector.empty,
                         network: EditNetwork = Graph.empty)

  final case class Props(logo: String, namer: Namer[IO])

  final class Backend($ : BackendScope[Props, State]) {

    private val editorRef = Ref[html.Element]

    private def findConnector(
      port: EditPort,
      connections: Vector[EditConnection]
    ): Option[EditPort] =
      connections
        .find(containsId(port.meta.id)(_))
        .flatMap(connectsTo(port.meta.id)(_))

    private def filterConnector(
      port: EditPort,
      connections: Vector[EditConnection]
    ): Vector[EditConnection] =
      connections.filter(!containsId(port.meta.id)(_))

    private def addNode(nodeType: NodeType): Callback =
      for {
        props <- $.props
        node <- Nodes.mkNode(nodeType, props.namer).toCallback
        _ <- $.modState(state => {
          state.copy(nodes = state.nodes :+ node)
        })
      } yield ()

    private def deleteNode(nodeId: String): Callback =
      $.modState(state => {
        state.copy(nodes = state.nodes.filter(_.meta.id != nodeId))
      })

    private def bringToFront(nodeId: String): Callback =
//      $.modState(state => {
//        val split = state.nodes.span(_._1 != nodeId)
//        state.copy(nodes = split match {
//          case (front, node +: back) => front ++ back :+ node
//          case (front, node)         => front ++ node
//        })
//      })
      Callback(println(nodeId))

    private def adjustPorts(ports: Vector[EditPort]): Callback =
      $.modState(state => {
        val adjusted = state.connections.map {
          case c @ Connection(Port(meta: PortMeta, _), port)
              if ports.exists(_.meta.id == meta.id) =>
            ports
              .find(_.meta.id == meta.id)
              .map(
                p =>
                  Connection(
                    EditPort(
                      PortMeta(
                        meta.id,
                        Anchor(
                          state.offset.x + p.meta.anchor.x,
                          state.offset.y + p.meta.anchor.y,
                          p.meta.anchor.orientation
                        )
                      ),
                      p.name
                    ),
                    port
                )
              )
              .getOrElse(c)
          case c @ Connection(port, Port(meta: PortMeta, _)) =>
            ports
              .find(_.meta.id == meta.id)
              .map(
                p =>
                  Connection(
                    port,
                    EditPort(
                      PortMeta(
                        meta.id,
                        Anchor(
                          state.offset.x + p.meta.anchor.x,
                          state.offset.y + p.meta.anchor.y,
                          p.meta.anchor.orientation
                        )
                      ),
                      p.name
                    )
                )
              )
              .getOrElse(c)
          case c => c
        }
        state.copy(connections = adjusted)
      })

    private def deletePorts(ports: Vector[String]): Callback =
      $.modState(state => {
        val remaining =
          state.connections.filter(
            (c: EditConnection) =>
              !ports.contains(c.from.meta.id) && !ports
                .contains(c.to.meta.id)
          )
        state.copy(connections = remaining)
      })

    private def onPortClick(port: EditPort): Callback =
      $.modState(state => {
        val portX = state.offset.x + port.meta.anchor.x
        val portY = state.offset.y + port.meta.anchor.y
        val currentPort =
          EditPort(
            PortMeta(
              port.meta.id,
              Anchor(portX, portY, port.meta.anchor.orientation)
            ),
            port.name
          )

        state.connectionState match {
          case Connecting(from, _) =>
            if (currentPort.meta.id == from.meta.id)
              state.copy(connectionState = NotConnecting)
            else if (currentPort.meta.id == from.meta.id)
              state
            else {
              findConnector(currentPort, state.connections) match {
                case Some(connectedPort) =>
                  val connection = Connection(from, currentPort)
                  val connections = filterConnector(
                    currentPort,
                    state.connections
                  ) :+ connection
                  state.copy(
                    connectionState =
                      Connecting(connectedPort, Some(currentPort.meta.anchor)),
                    connections = connections
                  )
                case None =>
                  val connection = Connection(from, currentPort)
                  val connections = state.connections :+ connection
                  state.copy(
                    connectionState = NotConnecting,
                    connections = connections
                  )
              }
            }
          case NotConnecting =>
            findConnector(currentPort, state.connections) match {
              case Some(connectedPort) =>
                state.copy(
                  connectionState =
                    Connecting(connectedPort, Some(currentPort.meta.anchor)),
                  connections = filterConnector(currentPort, state.connections)
                )
              case None =>
                state.copy(connectionState = Connecting(currentPort, None))
            }
        }
      })

    private def onPortHover(port: EditPort): Callback =
      $.modState(state => {
        state.connectionState match {
          case Connecting(from, _) if port.meta.id == from.meta.id =>
            state
          case Connecting(from, _) =>
            state.copy(
              connectionState = Connecting(from, Some(port.meta.anchor))
            )
          case NotConnecting => state
        }
      })

    private val updateOffset =
      editorRef.foreachCB(editor => {
        val xOffset = editor.scrollLeft
        val yOffset = editor.scrollTop
        $.modState(_.copy(offset = Point(xOffset, yOffset)))
      })

    def render(props: Props, state: State): VdomElement =
      NodeMenu(
        props.logo,
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
                    ^.key := n.meta.id,
                    NodeContainer(
                      n.meta.id,
                      n match {
                        case InputNode(_)        => Input
                        case OutputNode(_)       => Output
                        case ProcessorNode(_, _) => Processor

                      },
                      props.namer,
                      onPortClick,
                      onPortHover,
                      adjustPorts,
                      deletePorts,
                      deleteNode(n.meta.id),
                      bringToFront(n.meta.id)
                    )
                )
              ): _*
            ),
            //Connectors
            state.connections
              .toTagMod(c => Connector(c.from.meta.anchor, c.to.meta.anchor)),
            state.connectionState match {
              case Connecting(from, to) =>
                InFlightConnector(from.meta.anchor, to)
              case NotConnecting => EmptyVdom
            },
            //Event Listeners
            ^.onScroll --> updateOffset
          )
        )
      )
  }

  private val component =
    ScalaComponent
      .builder[Props]("GraphEditor")
      .initialState(State(NotConnecting, Point(0, 0)))
      .renderBackend[Backend]
      .build

  def apply(logo: String, namer: Namer[IO]): Unmounted[Props, State, Backend] =
    component(Props(logo, namer))
}
