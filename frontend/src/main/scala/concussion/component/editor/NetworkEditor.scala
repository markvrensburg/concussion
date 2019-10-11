package concussion
package component
package editor

import cats.implicits._
import cats.effect.IO
import Connections._
import concussion.domain.Multiple
import concussion.domain.Node.ProcessorNode
import concussion.geometry.{Anchor, Point}
import concussion.graph.LGraph
import concussion.styles.{GraphStyle, NodeStyle, PageStyle}
import concussion.util.{Namer, Nodes, Ports}
import japgolly.scalajs.react.CatsReact._
import concussion.util.CatsIOReact._
import japgolly.scalajs.react.Ref.Simple
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._

object NetworkEditor {

  sealed trait ConnectionState
  final case class Connecting(from: ConnectionPoint, to: Option[Anchor] = None)
      extends ConnectionState
  case object NotConnecting extends ConnectionState

  final case class State(connectionState: ConnectionState = NotConnecting,
                         offset: Point = Point(0, 0),
                         topNodeId: Option[String] = None,
                         network: EditNetwork = LGraph.empty)

  final case class Props(logo: String, namer: Namer[IO])

  final class Backend($ : BackendScope[Props, State]) {

    private val editorRef = Ref[html.Element]

    private def getPoint(ref: Simple[html.Element]) =
      ref.get
        .map(e => {
          val rect = e.getBoundingClientRect
          (
            rect.left + ((rect.right - rect.left) / 2),
            rect.top + ((rect.bottom - rect.top) / 2)
          )
        })
        .asCallback
//
//    private def getPorts(node: EditNode) =
//      node.ports.toChain
//        .map(
//          port =>
//            getPoint(port.meta._2)
//              .map(_.map(p => (port.id, Anchor(p._1, p._2, port.meta._1))))
//        )
//        .sequence
//        .map(_.mapFilter(identity))
//

    private def mkConnection(from: ConnectionPoint,
                             to: ConnectionPoint,
                             network: EditNetwork) = {
      //todo avoid duplicate nodes
      val edges = network.edgeSet
      val connection = edges.find(e => {
        val ids = Set(e._2.id, e._3.id)
        ids.contains(from.nodeId) && ids.contains(to.nodeId)
      }) match {
        case Some(edge) =>
          LGraph.connect(
            edge._1 |+| Connections(Set(Connection(from, to))),
            LGraph.vertex(edge._2),
            LGraph.vertex(edge._3)
          ): EditNetwork
        case _ => LGraph.empty: EditNetwork
      }
      network.induceV(n => (n.id != from.nodeId) || (n.id != to.nodeId)) + connection
    }

    private def findConnection(nodeId: String,
                               portId: String,
                               network: EditNetwork) =
      network.edgeSet
        .find(es => es._1.containsConnection(portId))
        .flatMap(
          es =>
            (es._2.ports.toChain ++ es._3.ports.toChain)
              .find(_.id == portId) // todo make use of nodeId in search
        )
        .map(
          p =>
            getPoint(p.meta._2).map(
              _.map(
                pt =>
                  ConnectionPoint(
                    nodeId,
                    portId,
                    Anchor(pt._1, pt._2, p.meta._1)
                )
              )
          )
        )
        .getOrElse(CallbackTo(None))

    private def removeConnection(portId: String, network: EditNetwork) =
      network.mapE(e => e.removeConnection(portId))

    private def addNode(node: EditNode): Callback =
      $.modState(
        state =>
          state.copy(
            topNodeId = Some(node.id),
            network = state.network + LGraph.vertex(node)
        )
      )

    private def cloneNode(nodeId: String): Callback =
      for {
        props <- $.props
        state <- $.state
        node = state.network.vertexSet.find(_.id == nodeId).get
        newNode <- Nodes.copyNode(node, props.namer).toCallback
        _ <- addNode(newNode)
      } yield ()

    private def updateCode(nodeId: String)(newCode: String) =
      $.modState(
        state =>
          state.copy(network = state.network.mapV {
            case n @ ProcessorNode(id, _, _, _) if id == nodeId =>
              n.copy(code = newCode)
            case n => n
          })
      )

    private def newPort(node: EditNode): Callback =
      for {
        props <- $.props
        port <- Ports
          .mkPort(node.id, Nodes.getType(node), props.namer)
          .toCallback
        _ <- $.modState(
          state =>
            state.copy(
              network = state.network.mapV(
                n =>
                  if (n.id == node.id) {
                    n.mapPorts {
                      case Multiple(ports) =>
                        Multiple(ports :+ port)
                      case p => p
                    }
                  } else n
              )
          )
        )
      } yield ()

    private def modifyPorts(nodeId: String)(f: EditPort => EditPort): Callback =
      $.modState(
        state =>
          state.copy(
            network = state.network.mapV(
              n =>
                if (n.id == nodeId) {
                  n.mapPorts(_.map(f))
                } else n
            )
        )
      )

    private def deletePort(nodeId: String)(portId: String): Callback =
      // todo remove occurrence from edges
      $.modState(
        state =>
          state.copy(
            connectionState = NotConnecting,
            network = state.network.mapV(
              n =>
                if (n.id == nodeId) {
                  n.mapPorts {
                    case Multiple(ports) =>
                      Multiple(ports.filter(_.id != portId))
                    case p => p
                  }
                } else n
            )
        )
      )

    private def deleteNode(nodeId: String): Callback =
      // todo remove occurrence from edges
      $.modState(state => {
        state.copy(
          connectionState = NotConnecting,
          network = state.network.induceV(_.id != nodeId)
        )
      })

    private def bringToFront(nodeId: String): Callback =
      $.modState(_.copy(topNodeId = Some(nodeId)))

//    private def adjustPorts(ports: List[EditPort]): Callback =
//      $.modState(
//        state =>
//          state.copy(network = state.network.map(v => {
//            ports
//              .find(_.id == v._1.id)
//              .map(p => {
//                (
//                  EditPort(
//                    PortMeta(
//                      v._1.id,
//                      Anchor(
//                        state.offset.x + p.meta._1.anchor.x,
//                        state.offset.y + p.meta._1.anchor.y,
//                        p.meta._1.anchor.orientation
//                      )
//                    ),
//                    p.meta._2,
//                    p.name
//                  ),
//                  v._2
//                )
//              })
//              .getOrElse(v)
//          }))
//      )
//
//    private def deletePorts(ports: List[String]): Callback =
//      $.modState(
//        state =>
//          state.copy(
//            connectionState = NotConnecting,
//            network = state.network.induce(v => !ports.contains(v._1.id))
//        )
//      )
//
    private def onPortClick(nodeId: String)(port: EditPort): Callback =
      for {
        state <- $.state
        anchor <- getPoint(port.meta._2)
          .map(
            _.map(
              p =>
                Anchor(
                  state.offset.x + p._1,
                  state.offset.y + p._2,
                  port.meta._1
              )
            )
          )
        _ <- state.connectionState match {
          case Connecting(from, _) if port.id == from.portId =>
            $.modState(_.copy(connectionState = NotConnecting)) //Connect to same port -> cancel connecting
          case Connecting(from, _) if nodeId == from.nodeId =>
            Callback.empty //Connect to same node -> no effect
          case Connecting(from, _) =>
            for {
              found <- findConnection(nodeId, port.id, state.network)
              _ <- found match {
                case Some(connectionPoint) =>
                  Callback(println(connectionPoint))
                case None =>
                  //No existing connection on clicked port
                  val connection =
                    mkConnection(
                      from,
                      ConnectionPoint(nodeId, port.id, anchor.get),
                      state.network
                    )
                  $.modState(
                    _.copy(
                      connectionState = NotConnecting,
                      network = connection
                    )
                  )
              }
            } yield ()
          //              findConnector(currentPort, state.network.edgeSet) match {
          //                case Some(connectedPort) =>
          //                  //Current port is connected
          //                  val connection =
          //                    connectPorts(from, currentPort, state.network)
          //                  state.copy(
          //                    connectionState = Connecting(
          //                      connectedPort,
          //                      Some(currentPort.meta._1.anchor)
          //                    ),
          //                    network = filterConnector(currentPort, state.network) + connection
          //                  )
          //                case None =>
          //                  //No existing connection on current port
          //                  val connection =
          //                    connectPorts(from, currentPort, state.network)
          //                  state.copy(
          //                    connectionState = NotConnecting,
          //                    network = state.network + connection
          //                  )
          //              }
          //            }
          case NotConnecting =>
            for {
              found <- findConnection(nodeId, port.id, state.network)
              _ <- found match {
                case Some(connectionPoint) =>
                  $.modState(
                    _.copy(
                      connectionState = Connecting(connectionPoint, anchor),
                      network = removeConnection(port.id, state.network)
                    )
                  )
                case None =>
                  $.modState(
                    _.copy(
                      connectionState =
                        Connecting(ConnectionPoint(nodeId, port.id, anchor.get))
                    )
                  )
              }
            } yield ()
        }
      } yield ()

    private def onPortHover(port: EditPort): Callback =
      for {
        anchor <- getPoint(port.meta._2)
          .map(_.map(p => Anchor(p._1, p._2, port.meta._1)))
        _ <- $.modState(
          state =>
            state.connectionState match {
              case Connecting(from, _) if port.id == from.portId =>
                state
              case Connecting(from, _) =>
                state.copy(connectionState = Connecting(from, anchor))
              case NotConnecting => state
          }
        )
      } yield ()

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
        addNode,
        <.div(
          PageStyle.editor,
          <.div.withRef(editorRef)(
            GraphStyle.graphEditor,
            ^.id := GraphStyle.nodeEditorId,
            Infobar(),
            //Nodes
            React.Fragment(
              state.network.vertexSet //todo use vertexVector
                .map(
                  n =>
                    <.div(
                      ^.key := n.id,
                      ^.zIndex := {
                        state.topNodeId match {
                          case Some(id) if n.id == id =>
                            NodeStyle.topNodeZIndex
                          case _ => NodeStyle.bottomNodeZIndex
                        }
                      },
                      NodeContainer(
                        n,
                        props.namer,
                        onPortClick(n.id), //onPortClick: EditPort => Callback,
                        onPortHover,
                        newPort(n),
                        modifyPorts(n.id),
                        updateCode(n.id),
                        deletePort(n.id), //deletePorts: (String,Chain[String]) => Callback,
                        cloneNode(n.id),
                        deleteNode(n.id),
                        bringToFront(n.id)
                      )
                  )
                )
                .toList: _*
            ),
            //Connectors
            state.network.simpleEdgeSet
              .flatMap(_.connections)
              .toTagMod(c => Connector(c.from.anchor, c.to.anchor)),
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

  private val component =
    ScalaComponent
      .builder[Props]("NetworkEditor")
      .initialState(State())
      .renderBackend[Backend]
      .build

  def apply(logo: String, namer: Namer[IO]): Unmounted[Props, State, Backend] =
    component(Props(logo, namer))
}
