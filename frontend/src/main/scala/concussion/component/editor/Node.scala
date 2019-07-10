package concussion.component.editor

import cats.effect.IO
import concussion.facade.ace.{AceEditor, EditorProps}
import concussion.facade.draggable.{Draggable, DraggableBounds, Grid}
import concussion.styles.NodeStyle
import concussion.util.Namer
import concussion.util.CatsReact._
import enum.Enum
import japgolly.scalajs.react.Ref.Simple
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import react.semanticui.colors.{Blue, Green, Red}
import react.semanticui.elements.header.Header
import react.semanticui.elements.icon.Icon
import react.semanticui.elements.segment.{Segment, SegmentAttached}
import react.semanticui.textalignment.Center
import scalacss.ScalaCssReact._

sealed trait NodeType
case object Input extends NodeType
case object Output extends NodeType
case object Processor extends NodeType

object NodeType {
  val nodeTypes: Enum[NodeType] = Enum.derived[NodeType]
}

object Node {

  final case class State(ports: Vector[(String, Simple[html.Element])] = Vector.empty)

  final case class Props(
      id: String,
      nodeType: NodeType,
      namer: Namer[IO],
      onPortClick: Port => Callback,
      onPortHover: PortOrientation => Callback,
      adjustPorts: Set[Port] => Callback
  )

  final class Backend($ : BackendScope[Props, State]) {

    private def addPort: Callback =
      for {
        props <- $.props
        id <- props.namer.nextName(s"${props.id}_Port").toCallback
        _ <- $.modState(state => {
          state.copy(ports = state.ports :+ (id -> Ref[html.Element]))
        })
      } yield ()

    private val bounds = DraggableBounds(-199, null, 0, null)

    private def input(props: Props, state: State) =
      Draggable(
        props.id,
        Draggable
          .props(grid = Grid(5, 5), handle = ".dragger", bounds = bounds, onDrag = updateDrag),
        <.div(
          //          ^.left := "50%",
          //          ^.top := "50%",
          //          ^.transform := "translate(-50%,-50%)",
          NodeStyle.nodePos,
          ^.id := props.id,
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
            //Ports
            <.div(
              state.ports.toList.toTagMod(
                p => PortContainer(p._1, "Port", Right, p._2, props.onPortClick, props.onPortHover)
              )
            ),
            <.div(
              ^.width := "100%",
              ^.display := "flex",
              ^.justifyContent := "center",
              ^.marginTop := ".25rem",
              <.div(
                ^.onClick --> addPort,
                Icon(Icon.props(name = "plus circle", link = true))
              )
            )
          )
        )
      )

    private def output(props: Props, state: State) =
      Draggable(
        props.id,
        Draggable
          .props(grid = Grid(5, 5), handle = ".dragger", bounds = bounds, onDrag = updateDrag),
        <.div(
          NodeStyle.nodePos,
          ^.id := props.id,
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
            //Ports
            <.div(
              state.ports.toList.toTagMod(
                p => PortContainer(p._1, "Port", Left, p._2, props.onPortClick, props.onPortHover)
              )
            ),
            <.div(
              ^.width := "100%",
              ^.display := "flex",
              ^.justifyContent := "center",
              ^.marginTop := ".25rem",
              <.div(
                ^.onClick --> addPort,
                Icon(Icon.props(name = "plus circle", link = true))
              )
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

    //    private val updateCode: AceEditor.OnChange =
    //      (e: ReactEvent) => Callback(println(e.toString))

    private val updateDrag: Draggable.DraggableEventHandler =
      (mouse, data) => Callback(println(s"${mouse.clientX},${mouse.clientY}; ${data.x},${data.y}"))

    private def processor(props: Props, state: State) =
      Draggable(
        props.id,
        Draggable
          .props(
            grid = Grid(5, 5),
            handle = ".dragger",
            bounds = bounds,
            onDrag = updateDrag
          ),
        <.div(
          NodeStyle.nodePos,
          ^.id := props.id,
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
            <.div(
              ^.height := "100%",
              ^.minWidth := "210px",
              AceEditor(
                AceEditor.props(
                  width = "100%",
                  mode = "yaml",
                  theme = "merbivore",
                  value = defaultText,
                  //onChange = updateCode,
                  minLines = defaultText.lines.size,
                  maxLines = defaultText.lines.size,
                  wrapEnabled = true,
                  debounceChangePeriod = 500,
                  editorProps = EditorProps(blockScrolling = true)
                )
              )
            )
          ),
          Segment(
            Segment.props(inverted = true, compact = true, attached = SegmentAttached.Bottom),
            //Ports
            <.div(
              state.ports.toList.toTagMod(
                p => PortContainer(p._1, "Port", Left, p._2, props.onPortClick, props.onPortHover)
              )
            ),
            <.div(
              ^.width := "100%",
              ^.display := "flex",
              ^.justifyContent := "center",
              ^.marginTop := ".25rem",
              <.div(
                ^.onClick --> addPort,
                Icon(Icon.props(name = "plus circle", link = true))
              )
            )
          )
        )
      )

    def render(props: Props, state: State): VdomElement = props.nodeType match {
      case Input     => input(props, state)
      case Output    => output(props, state)
      case Processor => processor(props, state)
    }
  }

  private val component =
    ScalaComponent
      .builder[Props]("NodeEditor")
      .initialStateCallbackFromProps(
        props =>
          props.namer
            .nextName(s"${props.id}_Port")
            .toCallback
            .map(name => State(ports = Vector(name -> Ref[html.Element])))
      )
      .renderBackend[Backend]
      .build

  def apply(
      id: String,
      nodeType: NodeType,
      namer: Namer[IO],
      onPortClick: Port => Callback = _ => Callback.empty,
      onPortHover: PortOrientation => Callback = _ => Callback.empty,
      adjustPorts: Set[Port] => Callback = _ => Callback.empty
  ): Unmounted[Props, State, Backend] =
    component(Props(id, nodeType, namer, onPortClick, onPortHover, adjustPorts))
}
