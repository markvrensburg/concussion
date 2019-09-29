package concussion
package component
package editor

import cats.implicits._
import cats.effect.IO
import concussion.geometry.{Anchor, Point}
import concussion.domain._
import concussion.graph.Graph
import concussion.styles.{GraphStyle, NodeStyle, PageStyle}
import concussion.util.{Namer, Nodes, Ports}
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
                         topNodeId: Option[String] = None,
                         network: EditNetwork = Graph.empty)

  final case class Props(logo: String, namer: Namer[IO])

  final class Backend($ : BackendScope[Props, State]) {

    private val editorRef = Ref[html.Element]

    private def findConnector(
      port: EditPort,
      connections: Set[EditConnection]
    ): Option[EditPort] =
      connections
        .find(containsId(port.meta._1.id)(_))
        .flatMap(connectsTo(port.meta._1.id)(_))

    private def filterConnector(port: EditPort,
                                network: EditNetwork): EditNetwork =
      network.removeEdge((v1, v2) => !containsId(port.meta._1.id)((v1, v2)))

    private def connectPorts(port1: EditPort,
                             port2: EditPort,
                             network: EditNetwork): EditNetwork = {
      val res: EditNetwork = (
        network.vertexSet.find(_._1.meta._1.id == port1.meta._1.id),
        network.vertexSet.find(_._1.meta._1.id == port2.meta._1.id)
      ) match {
        case (Some(v1), Some(v2)) => Graph.vertex(v1) * Graph.vertex(v2)
        case _                    => Graph.empty
      }

      println(res)
      println(network.vertexSet)
      println(network.vertexSet)

      res
    }

    private def distinctNodes(network: EditNetwork): List[EditNode] =
      network.vertexList.groupBy(_._2.meta.id).values.map(_.head._2).toList

    private def portsForNode(nodeId: String,
                             network: EditNetwork): Set[EditPort] =
      network.vertexSet.filter(_._2.meta.id == nodeId).map(_._1)

    private def nodeForPort(portId: String,
                            network: EditNetwork): Option[EditNode] =
      network.vertexSet.find(_._1.meta._1.id == portId).map(_._2)

    private def addVertices(node: EditNode, ports: List[EditPort]): Callback =
      $.modState(
        state =>
          state.copy(
            topNodeId = Some(node.meta.id),
            network = state.network + Graph.vertices(ports.map((_, node)))
        )
      )

    private def cloneNode(nodeId: String): Callback =
      for {
        props <- $.props
        state <- $.state
        existingNode = state.network.vertexSet.find(_._2.meta.id == nodeId).get
        existingPorts = portsForNode(nodeId, state.network).toList
        node <- Nodes.copyNode(existingNode._2, props.namer).toCallback
        ports <- existingPorts
          .traverse(Ports.copyPort(_, node, props.namer))
          .toCallback
        _ <- addVertices(node, ports)
      } yield ()

    private def deleteNode(nodeId: String): Callback =
      $.modState(state => {
        state.copy(
          connectionState = NotConnecting,
          network = state.network.induce(_._2.meta.id != nodeId)
        )
      })

    private def bringToFront(nodeId: String): Callback =
      $.modState(_.copy(topNodeId = Some(nodeId)))

    private def adjustPorts(ports: List[EditPort]): Callback =
      $.modState(
        state =>
          state.copy(network = state.network.map(v => {
            ports
              .find(_.meta._1.id == v._1.meta._1.id)
              .map(p => {
                (
                  EditPort(
                    PortMeta(
                      v._1.meta._1.id,
                      Anchor(
                        state.offset.x + p.meta._1.anchor.x,
                        state.offset.y + p.meta._1.anchor.y,
                        p.meta._1.anchor.orientation
                      )
                    ),
                    p.meta._2,
                    p.name
                  ),
                  v._2
                )
              })
              .getOrElse(v)
          }))
      )

    private def deletePorts(ports: List[String]): Callback =
      $.modState(
        state =>
          state.copy(
            connectionState = NotConnecting,
            network =
              state.network.induce(v => !ports.contains(v._1.meta._1.id))
        )
      )

    private def onPortClick(port: EditPort): Callback =
      $.modState(state => {
        val portX = state.offset.x + port.meta._1.anchor.x
        val portY = state.offset.y + port.meta._1.anchor.y
        val currentPort =
          EditPort(
            PortMeta(
              port.meta._1.id,
              Anchor(portX, portY, port.meta._1.anchor.orientation)
            ),
            port.meta._2,
            port.name
          )

        state.connectionState match {
          case Connecting(from, _) =>
            if (currentPort.meta._1.id == from.meta._1.id)
              state.copy(connectionState = NotConnecting) //Same port connected to -> cancel connecting
            else if (nodeForPort(currentPort.meta._1.id, state.network) == nodeForPort(
                       from.meta._1.id,
                       state.network
                     ))
              state //Same node connected to -> no effect
            else {
              findConnector(currentPort, state.network.edgeSet) match {
                case Some(connectedPort) =>
                  //Current port is connected
                  val connection =
                    connectPorts(from, currentPort, state.network)
                  state.copy(
                    connectionState = Connecting(
                      connectedPort,
                      Some(currentPort.meta._1.anchor)
                    ),
                    network = filterConnector(currentPort, state.network) + connection
                  )
                case None =>
                  //No existing connection on current port
                  val connection =
                    connectPorts(from, currentPort, state.network)
                  state.copy(
                    connectionState = NotConnecting,
                    network = state.network + connection
                  )
              }
            }
          case NotConnecting =>
            findConnector(currentPort, state.network.edgeSet) match {
              case Some(connectedPort) =>
                state.copy(
                  connectionState =
                    Connecting(connectedPort, Some(currentPort.meta._1.anchor)),
                  network = filterConnector(currentPort, state.network)
                )
              case None =>
                state.copy(connectionState = Connecting(currentPort, None))
            }
        }
      })

    private def onPortHover(port: EditPort): Callback =
      $.modState(state => {
        state.connectionState match {
          case Connecting(from, _) if port.meta._1.id == from.meta._1.id =>
            state
          case Connecting(from, _) =>
            state.copy(
              connectionState = Connecting(from, Some(port.meta._1.anchor))
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
        props.namer,
        addVertices,
        <.div(
          PageStyle.editor,
          <.div.withRef(editorRef)(
            GraphStyle.graphEditor,
            ^.id := GraphStyle.nodeEditorId,
            Infobar(),
            //Nodes
            React.Fragment(
              distinctNodes(state.network)
                .map(
                  n =>
                    <.div(
                      ^.key := n.meta.id,
                      ^.zIndex := {
                        state.topNodeId match {
                          case Some(id) if n.meta.id == id =>
                            NodeStyle.topNodeZIndex
                          case _ => NodeStyle.bottomNodeZIndex
                        }
                      },
                      NodeContainer(
                        n,
                        portsForNode(n.meta.id, state.network),
                        props.namer,
                        onPortClick,
                        onPortHover,
                        adjustPorts,
                        deletePorts,
                        addVertices,
                        cloneNode(n.meta.id),
                        deleteNode(n.meta.id),
                        bringToFront(n.meta.id)
                      )
                  )
                ): _*
            ),
            //Connectors
            state.network.edgeSet
              .toTagMod(
                c => Connector(c._1._1.meta._1.anchor, c._2._1.meta._1.anchor)
              ),
            state.connectionState match {
              case Connecting(from, to) =>
                InFlightConnector(from.meta._1.anchor, to)
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
