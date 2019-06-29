package concussion.component.editor

import concussion.facade.ace.AceEditor
import concussion.facade.draggable.{Draggable, DraggableBounds, Grid}
import concussion.styles.PageStyle
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import org.scalajs.dom.html
import react.semanticui.colors.{Blue, Green, Red}
import react.semanticui.elements.header.Header
import react.semanticui.elements.icon.Icon
import react.semanticui.elements.segment.{Segment, SegmentAttached}
import react.semanticui.textalignment.Center
import scalacss.ScalaCssReact._

object NodeEditor {

  sealed trait ConnectionState
  final case class Connecting(from: Port, to: Port) extends ConnectionState
  case object NotConnecting extends ConnectionState

  final case class Connection(port1: Port, port2: Port)

  final case class State(
      connectionState: ConnectionState,
      offset: (Double, Double),
      connections: Set[Connection] = Set.empty
  )

  final class Backend($ : BackendScope[Unit, State]) {

    private val bounds = DraggableBounds(-199, null, 0, null)

    private val editorRef = Ref[html.Element]

    private val input =
      Draggable(
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
            PortContainer("Port1", Right),
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

    private val output =
      Draggable(
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
            PortContainer("Port1", Left),
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

    private val processor =
      Draggable(
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
            PortContainer("Port1", Left),
            PortContainer("Port2", Right),
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

    private def updateConnectionState(e: ReactMouseEvent) = {
      val x = e.clientX
      val y = e.clientY
      $.modState(state => {
        state.connectionState match {
          case Connecting(from, to) => {
            val connections = state.connections + Connection(from, to)
            state.copy(connectionState = NotConnecting, connections = connections)
          }
          case NotConnecting => {
            val portX = state.offset._1 + x
            val portY = state.offset._2 + y
            state.copy(
              connectionState = Connecting(Port(portX, portY, Right), Port(portX, portY, None))
            )
          }
        }
      })
    }

    private def updateConnection(e: ReactMouseEvent) = {
      val x = e.clientX
      val y = e.clientY
      $.modState(state => {
        state.connectionState match {
          case Connecting(from, _) =>
            state.copy(
              connectionState = Connecting(
                from,
                Port(state.offset._1 + x, state.offset._2 + y, None)
              )
            )
          case NotConnecting => state
        }
      })
    }

    def render(state: State): VdomElement =
      NodeMenu(
        <.div(
          PageStyle.editor,
          <.div.withRef(editorRef)(
            PageStyle.nodeEditor,
            ^.id := "node-editor",
            Infobar(),
            Toolbar(),
            //Nodes
            input,
            processor,
            output,
            //Connectors
            state.connections.toTagMod(c => Connector(c.port1, c.port2)),
            state.connectionState match {
              case Connecting(from, to) => Connector(from, to)
              case NotConnecting        => React.Fragment()
            },
            //Event Listeners
            ^.onScroll --> updateOffset,
            ^.onMouseUp ==> updateConnectionState,
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
      .builder[Unit]("NodeEditor")
      .initialState(State(NotConnecting, (0, 0)))
      .renderBackend[Backend]
      .build

  def apply(): Unmounted[Unit, State, Backend] = component()
}
