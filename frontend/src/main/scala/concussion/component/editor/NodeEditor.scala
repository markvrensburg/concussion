package concussion.component.editor

import concussion.facade.ace.AceEditor
import concussion.facade.draggable.{Draggable, DraggableBounds, Grid}
import concussion.styles.PageStyle
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import react.semanticui.colors.{Blue, Green, Red}
import react.semanticui.elements.header.Header
import react.semanticui.elements.icon.Icon
import react.semanticui.elements.segment.{Segment, SegmentAttached}
import react.semanticui.textalignment.Center
import scalacss.ScalaCssReact._

object NodeEditor {

  final class Backend() {

    private val bounds = DraggableBounds(-199, null, 0, null)

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

    private val updateDrag: Draggable.DraggableEventHandler =
      (mouse, data) => Callback(println(s"${mouse.clientX},${mouse.clientY}; ${data.x},${data.y}"))

    private val processor =
      Draggable(
        Draggable
          .props(grid = Grid(5, 5), handle = ".dragger", bounds = bounds, onStop = updateDrag),
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

    def render: VdomElement =
      NodeMenu(
        <.div(
          PageStyle.editor,
          <.div(
            PageStyle.nodeEditor,
            ^.id := "node-editor",
            ^.onScroll ==> { e =>
              Callback(println(e.eventType))
            },
            ^.onMouseMove ==> { e =>
              Callback(println(e.eventType))
            },
            Infobar(),
            Toolbar(),
            Connector(300, 200, 601, 504),
            input,
            processor,
            processor,
            output
          )
        )
      )
  }

  private val component = ScalaComponent
    .builder[Unit]("NodeEditor")
    .renderBackend[Backend]
    .build

  def apply(): Unmounted[Unit, Unit, Backend] = component()
}
