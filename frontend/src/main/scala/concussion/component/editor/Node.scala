package concussion.component.editor

import cats.implicits._
import cats.effect.IO
import concussion.facade.ace.{AceEditor, EditorProps}
import concussion.facade.draggable.{Draggable, DraggableBounds, DraggableData, Grid}
import concussion.styles.NodeStyle
import concussion.util.Namer
import enum.Enum
import japgolly.scalajs.react.Ref.Simple
import japgolly.scalajs.react._
import japgolly.scalajs.react.CatsReact._
import concussion.util.CatsIOReact._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.{MouseEvent, html}
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

  final case class State(
      ports: Vector[(String, Simple[html.Element], PortOrientation)] = Vector.empty
  )

  final case class Props(
      id: String,
      nodeType: NodeType,
      namer: Namer[IO],
      onPortClick: Port => Callback,
      onPortHover: PortOrientation => Callback,
      adjustPorts: Vector[Port] => Callback
  )

  final class Backend($ : BackendScope[Props, State]) {

    private val getPorts =
      for {
        props <- $.props
        state <- $.state
        ports <- state.ports
          .map(p => {
            p._2.get
              .map(e => {
                val rect = e.getBoundingClientRect
                val center = (
                  rect.left + ((rect.right - rect.left) / 2),
                  rect.top + ((rect.bottom - rect.top) / 2)
                )
                Port(PortId(p._1, props.id), center._1, center._2, p._3)
              })
              .asCallback
              .map(_.get) //todo make this method safer
          })
          .sequence
      } yield ports

    private val addPort =
      for {
        props <- $.props
        id <- props.namer.nextName(s"${props.id}_Port").toCallback
        _ <- $.modState(state => {
          state.copy(
            ports = state.ports :+ (
              (
                id,
                Ref[html.Element],
                if (props.nodeType == Input) Right
                else Left
              )
            )
          )
        })
      } yield ()

    private val updateConnections =
      for {
        props <- $.props
        ports <- getPorts
        adjust <- props.adjustPorts(ports)
      } yield adjust

    private def deletePort(portId: PortId) =
      $.modState(state => {
        state.copy(ports = state.ports.filter(_._1 != portId.id)) //todo make id's type safe
      })

    private def shiftPort(portId: PortId) =
      $.modState(state => {
        val ports = state.ports.map {
          case (id, _, orientation) if id == portId.id => (id, Ref[html.Element], orientation.swap)
          case port                                    => port
        }
        state.copy(ports = ports)
      })

    private val bounds = DraggableBounds(-199, null, 0, null)

    private def input(props: Props, state: State) =
      Draggable(
        Draggable
          .props(
            grid = Grid(5, 5),
            handle = ".dragger",
            bounds = bounds,
            //onDrag = (_: MouseEvent, _: DraggableData) => updateConnections,
            onStart = (_: MouseEvent, _: DraggableData) => Callback(println("Starting")),
            onStop = (_: MouseEvent, _: DraggableData) => updateConnections
          ),
        <.div(
          //          ^.left := "50%",
          //          ^.top := "50%",
          //          ^.transform := "translate(-50%,-50%)",
          NodeStyle.nodePos,
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
            React.Fragment(
              state.ports.map(
                p =>
                  <.div(
                    ^.key := p._1,
                    PortContainer(
                      PortId(p._1, props.id),
                      "Port",
                      p._3,
                      p._2,
                      canDelete = true,
                      props.onPortClick,
                      props.onPortHover,
                      deletePort(PortId(p._1, props.id)),
                      shiftPort(PortId(p._1, props.id)),
                      updateConnections
                    )
                  )
              ): _*
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
        Draggable
          .props(
            grid = Grid(5, 5),
            handle = ".dragger",
            bounds = bounds,
            //onDrag = (_: MouseEvent, _: DraggableData) => updateConnections,
            onStop = (_: MouseEvent, _: DraggableData) => updateConnections
          ),
        <.div(
          NodeStyle.nodePos,
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
            React.Fragment(
              state.ports.map(
                p =>
                  <.div(
                    ^.key := p._1,
                    PortContainer(
                      PortId(p._1, props.id),
                      "Port",
                      p._3,
                      p._2,
                      canDelete = true,
                      props.onPortClick,
                      props.onPortHover,
                      deletePort(PortId(p._1, props.id)),
                      shiftPort(PortId(p._1, props.id)),
                      updateConnections
                    )
                  )
              ): _*
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

    private def processor(props: Props, state: State) =
      Draggable(
        Draggable
          .props(
            grid = Grid(5, 5),
            handle = ".dragger",
            bounds = bounds,
            //onDrag = (_: MouseEvent, _: DraggableData) => updateConnections,
            onStop = (_: MouseEvent, _: DraggableData) => updateConnections
          ),
        <.div(
          NodeStyle.nodePos,
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
            React.Fragment(
              state.ports.map(
                p =>
                  <.div(
                    ^.key := p._1,
                    PortContainer(
                      PortId(p._1, props.id),
                      "Port",
                      p._3,
                      p._2,
                      canDelete = true,
                      props.onPortClick,
                      props.onPortHover,
                      deletePort(PortId(p._1, props.id)),
                      shiftPort(PortId(p._1, props.id)),
                      updateConnections
                    )
                  )
              ): _*
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
      .builder[Props]("Node")
      .initialStateCallbackFromProps(
        props =>
          props.namer
            .nextName(s"${props.id}_Port")
            .toCallback
            .map(
              name =>
                State(
                  ports = Vector(
                    (
                      name,
                      Ref[html.Element],
                      if (props.nodeType == Input) Right
                      else Left
                    )
                  )
                )
            )
      )
      .renderBackend[Backend]
      .build

  def apply(
      id: String,
      nodeType: NodeType,
      namer: Namer[IO],
      onPortClick: Port => Callback = _ => Callback.empty,
      onPortHover: PortOrientation => Callback = _ => Callback.empty,
      adjustPorts: Vector[Port] => Callback = _ => Callback.empty
  ): Unmounted[Props, State, Backend] =
    component(Props(id, nodeType, namer, onPortClick, onPortHover, adjustPorts))
}
