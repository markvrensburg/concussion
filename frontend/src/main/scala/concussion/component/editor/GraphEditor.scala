package concussion
package component
package editor

import cats.effect.IO
import concussion.geometry.{Anchor, Neutral, Point}
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
                         network: EditNetwork = Graph.empty)

  final case class Props(logo: String, namer: Namer[IO])

  final class Backend($ : BackendScope[Props, State]) {

    private val editorRef = Ref[html.Element]

//    private def findConnector(
//      port: EditPort,
//      connections: List[EditConnection]
//    ): Option[EditPort] =
//      connections
//        .find(containsId(port.meta.id)(_))
//        .flatMap(connectsTo(port.meta.id)(_))

//    private def filterConnector(port: EditPort,
//                                network: EditNetwork): EditNetwork =
//      network.removeEdge((v1, v2) => !containsId(port.meta.id)((v1, v2)))

    private def distinctNodes(network: EditNetwork): List[EditNode] =
      network.vertexList.groupBy(_._2.meta.id).values.toList.map(_.head._2)

//    private def portsForNode(nodeId: String,
//                             network: EditNetwork): List[EditPort] =
//      network.vertexList.filter(_._2.meta.id == nodeId).map(_._1)

    private def addNode(nodeType: NodeType): Callback =
      for {
        props <- $.props
        newNode <- Nodes.mkNode(nodeType, props.namer).toCallback
        _ <- $.modState(state => {
          state.copy(
            network = state.network + (
              (
                EditPort(PortMeta("ppp", Anchor(0, 0, Neutral)), "PORT"),
                newNode
              )
            )
          )
        })
      } yield ()

//    private def addNode(node: EditNode, ports: List[EditPort]): Callback =
//      for {
//        props <- $.props
//        newNode <- Nodes.copyNode(node, props.namer).toCallback
//        _ <- $.modState(state => {
//          state.copy(network = state.network + Graph.vertices(ports.map((_,node))))
//        })
//      } yield ()

    private def deleteNode(nodeId: String): Callback =
      $.modState(state => {
        state.copy(network = state.network.induce(_._2.meta.id != nodeId))
      })

//    private def addPort(port: EditPort, node: EditNode): Callback =
//
//    private def deletePort(portId: String): Callback =

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
      $.modState(
        state =>
          state.copy(network = state.network.map(v => {
            ports
              .find(_.meta.id == v._1.meta.id)
              .map(p => {
                (
                  EditPort(
                    PortMeta(
                      v._1.meta.id,
                      Anchor(
                        state.offset.x + p.meta.anchor.x,
                        state.offset.y + p.meta.anchor.y,
                        p.meta.anchor.orientation
                      )
                    ),
                    p.name
                  ),
                  v._2
                )
              })
              .getOrElse(v)
          }))
      )

    private def deletePorts(ports: Vector[String]): Callback =
      $.modState(
        state =>
          state.copy(
            network = state.network.induce(v => !ports.contains(v._1.meta.id))
        )
      )

    private def onPortClick(port: EditPort): Callback =
      Callback(println(port))
//      $.modState(state => {
//        val portX = state.offset.x + port.meta.anchor.x
//        val portY = state.offset.y + port.meta.anchor.y
//        val currentPort =
//          EditPort(
//            PortMeta(
//              port.meta.id,
//              Anchor(portX, portY, port.meta.anchor.orientation)
//            ),
//            port.name
//          )
//
//        state.connectionState match {
//          case Connecting(from, _) =>
//            if (currentPort.meta.id == from.meta.id)
//              state.copy(connectionState = NotConnecting)
//            else if (currentPort.meta.id == from.meta.id)
//              state
//            else {
//              findConnector(currentPort, state.connections) match {
//                case Some(connectedPort) =>
//                  val connection = Connection(from, currentPort)
//                  val connections = filterConnector(
//                    currentPort,
//                    state.connections
//                  ) :+ connection
//                  state.copy(
//                    connectionState =
//                      Connecting(connectedPort, Some(currentPort.meta.anchor)),
//                    connections = connections
//                  )
//                case None =>
//                  val connection = Connection(from, currentPort)
//                  val connections = state.connections :+ connection
//                  state.copy(
//                    connectionState = NotConnecting,
//                    connections = connections
//                  )
//              }
//            }
//          case NotConnecting =>
//            findConnector(currentPort, state.connections.edgeList) match {
//              case Some(connectedPort) =>
//                state.copy(
//                  connectionState =
//                    Connecting(connectedPort, Some(currentPort.meta.anchor)),
//                  connections = filterConnector(currentPort, state.connections)
//                )
//              case None =>
//                state.copy(connectionState = Connecting(currentPort, None))
//            }
//        }
//      })

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
              distinctNodes(state.network)
                .map(
                  n =>
                    <.div(
                      ^.key := n.meta.id,
                      NodeContainer(
                        n.meta.id,
                        Nodes.getType(n),
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
            state.network.edgeList
              .toTagMod(
                c => Connector(c._1._1.meta.anchor, c._2._1.meta.anchor)
              ),
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
