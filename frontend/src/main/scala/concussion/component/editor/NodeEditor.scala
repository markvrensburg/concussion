package concussion.component.editor

import cats.effect.IO
import concussion.facade.ace.AceEditor
import concussion.facade.draggable.{Draggable, DraggableBounds, Grid}
import concussion.styles.PageStyle
import concussion.util.Namer
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import org.scalajs.dom.html
import react.semanticui.colors.{Blue, Green, Red}
import react.semanticui.elements.header.Header
import react.semanticui.elements.icon.Icon
import react.semanticui.elements.segment.{Segment, SegmentAttached}
import react.semanticui.textalignment.Center
import concussion.util.CatsReact._
import scalacss.ScalaCssReact._

import scala.util.Random

object NodeEditor {

  sealed trait ConnectionState
  final case class Connecting(from: Port, to: Port) extends ConnectionState
  case object NotConnecting extends ConnectionState

  final case class Connection(port1: Port, port2: Port)

  final case class State(
      connectionState: ConnectionState,
      offset: (Double, Double),
      connections: Set[Connection] = Set.empty,
      nodes: Map[String, NodeType] = Map.empty
  )

  final case class Props(random: Random, namer: Namer[IO])

  final class Backend($ : BackendScope[Props, State]) {

    private val bounds = DraggableBounds(-199, null, 0, null)

    private val editorRef = Ref[html.Element]

    private def addNode(props: Props)(nodeType: NodeType): Callback =
      for {
        id <- props.namer.nextName(NodeType.nodeTypes.encode(nodeType)).toCallback
        _ <- $.modState(state => {
          state.copy(nodes = state.nodes + (id -> nodeType))
        })
      } yield ()

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
              val connections = state.connections + connection
              state.copy(connectionState = NotConnecting, connections = connections)
            }

            //Check if "from" - cancel in flight
            //Check if in same node - do nothing
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

    private def input(key: String) =
      Draggable(
        key,
        Draggable.props(grid = Grid(5, 5), handle = ".dragger", bounds = bounds),
        <.div(
          PageStyle.nodePos,
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
            )
          ),
          Segment(
            Segment.props(
              inverted = true,
              compact = true,
              attached = SegmentAttached.Bottom,
              textAlign = Center
            ),
            PortContainer("Port1", Right, onPortClick, onPortHover),
            <.div(
              ^.width := "100%",
              ^.display := "flex",
              ^.justifyContent := "center",
              ^.marginTop := ".25rem",
              Icon(Icon.props(name = "plus circle", link = true))
            )
          )
        )
      )

    private def output(key: String) =
      Draggable(
        key,
        Draggable.props(grid = Grid(5, 5), handle = ".dragger", bounds = bounds),
        <.div(
          PageStyle.nodePos,
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
            )
          ),
          Segment(
            Segment.props(
              inverted = true,
              compact = true,
              attached = SegmentAttached.Bottom,
              textAlign = Center
            ),
            PortContainer("Port1", Left, onPortClick, onPortHover),
            <.div(
              ^.width := "100%",
              ^.display := "flex",
              ^.justifyContent := "center",
              ^.marginTop := ".25rem",
              Icon(Icon.props(name = "plus circle", link = true))
            )
          )
        )
      )

    private val defaultText = """# Keep track of loops
                                |MOV 5 ACC
                                |SAV
                                |MOV 3, ACC
                                |LOOP:
                                |SUB 1
                                |JEZ END
                                |SWP
                                |JMP LOOP
                                |END:""".stripMargin

    private val updateCode: AceEditor.OnChange =
      (e: ReactEvent) => Callback(println(e.toString))

//    private val updateDrag: Draggable.DraggableEventHandler =
//      (mouse, data) => Callback(println(s"${mouse.clientX},${mouse.clientY}; ${data.x},${data.y}"))

    private def processor(key: String) =
      Draggable(
        key,
        Draggable
          .props(
            grid = Grid(5, 5),
            handle = ".dragger",
            bounds = bounds /*, onStop = updateDrag*/
          ),
        <.div(
          PageStyle.nodePos,
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
            )
          ),
          Segment(
            Segment.props(inverted = true, compact = true, attached = SegmentAttached.Attached),
            AceEditor(
              AceEditor.props(
                width = "210px",
                mode = "yaml",
                theme = "merbivore",
                value = defaultText,
                onChange = updateCode,
                minLines = defaultText.lines.size,
                maxLines = defaultText.lines.size,
                debounceChangePeriod = 500
              )
            )
          ),
          Segment(
            Segment.props(inverted = true, compact = true, attached = SegmentAttached.Bottom),
            PortContainer("Port1", Left, onPortClick, onPortHover),
            PortContainer("Port2", Right, onPortClick, onPortHover),
            <.div(
              ^.width := "100%",
              ^.display := "flex",
              ^.justifyContent := "center",
              ^.marginTop := ".25rem",
              Icon(Icon.props(name = "plus circle", link = true))
            )
          )
        )
      )

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
                Port(state.offset._1 + x, state.offset._2 + y, to.orientation)
              )
            )
          case NotConnecting => state
        }
      })
    }

    def render(props: Props, state: State): VdomElement =
      NodeMenu(
        props.random,
        addNode(props),
        <.div(
          PageStyle.editor,
          <.div.withRef(editorRef)(
            PageStyle.nodeEditor,
            ^.id := "node-editor",
            Infobar(),
            Toolbar(),
            //Nodes
            state.nodes.toTagMod(
              n =>
                n._2 match {
                  case Input     => input(n._1)
                  case Output    => output(n._1)
                  case Processor => processor(n._1)
                }
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

  private val component =
    ScalaComponent
      .builder[Props]("NodeEditor")
      .initialState(State(NotConnecting, (0, 0)))
      .renderBackend[Backend]
      .build

  def apply(random: Random, namer: Namer[IO]): Unmounted[Props, State, Backend] =
    component(Props(random, namer))
}
