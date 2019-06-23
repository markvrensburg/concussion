package concussion
package routes

import concussion.component._
import concussion.component.editor._
import concussion.facade.ace.AceEditor
import concussion.styles.PageStyle
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import concussion.facade.draggable._
import react.semanticui.elements.header.Header
import react.semanticui.colors._
import react.semanticui.elements.icon._
import react.semanticui.textalignment._
import react.semanticui.elements.segment._
import react.semanticui.modules.popup.{Popup, PopupContent, PopupOn, PopupPosition}
import scalacss.ScalaCssReact._

import scala.scalajs.js
import scala.util.Random

sealed trait Page

object Page {

  case object Landing extends Page
  case object Editor extends Page
  case object NotFound extends Page

  def landing(r: Random)(ctl: RouterCtl[Page]): VdomElement =
    <.div(
      ^.id := "logo-wrapper",
      ctl.setOnClick(Editor),
      ^.dangerouslySetInnerHtml := Logo(r)
    )

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
          <.div(
            Input(defaultValue = "Port1", onChange = v => Callback(println(v))),
            Icon(Icon.props(name = "dot circle outline", className = "port-socket"))
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
          <.div(
            Icon(Icon.props(name = "dot circle outline", className = "port-socket")),
            Input(defaultValue = "Port1")
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

  private val portOptions: VdomNode =
    Icon(Icon.props(name = "setting", className = "port-options", color = Grey, link = true))

  private val portOptionsContent: VdomNode =
    <.div("Port Options")

  private def portOptionsPopup(popupPosition: PopupPosition): VdomNode =
    Popup(
      Popup.props(
        trigger = portOptions,
        position = popupPosition,
        content = PopupContent.props(content = portOptionsContent),
        on = PopupOn.Click,
        hideOnScroll = true
      )
    )

  private val processor =
    Draggable(
      Draggable.props(grid = Grid(5, 5), handle = ".dragger", bounds = bounds, onStop = updateDrag),
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
          <.div(
            Icon(Icon.props(name = "dot circle outline", className = "port-socket")),
            Input(defaultValue = "Port1"),
            portOptionsPopup(PopupPosition.RightCenter)
          ),
          <.div(
            portOptionsPopup(PopupPosition.LeftCenter),
            Input(defaultValue = "Port2"),
            Icon(Icon.props(name = "dot circle outline", className = "port-socket"))
          )
        )
      )
    )

  def editor: VdomElement =
    NodeMenu(
      <.div(
        PageStyle.editor,
        <.div(
          PageStyle.nodeEditor,
          Infobar(),
          Toolbar(),
          //Connector(200,201,503,501),
          input,
          input,
          processor,
          processor,
          processor,
          output,
          output
        )
      )
    )

  def notFound(r: Random, size: (Int, Int)): VdomElement = {

    def c1 = r.nextInt(360)
    def c2 = c1 + r.nextInt(20)

    def background() =
      s"linear-gradient(to right, hsl($c1, 50%, 10%), hsl($c2, 40%, 50%))"

    def logo =
      <.div(
        ^.cls := "logo-container",
        ^.background := background,
        ^.borderRadius := "5px",
        ^.dangerouslySetInnerHtml := Logo(r)
      )

    val columns = js.Dictionary(
      "grid-template-columns" -> (1 to size._1).map(_ => "auto").mkString(" ")
    )

    <.div(
      ^.id := "logo-grid",
      ^.style := columns,
      React.Fragment((1 to (size._1 * size._2)).map(_ => logo): _*)
    )
  }
}
