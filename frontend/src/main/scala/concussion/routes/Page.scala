package concussion
package routes

import concussion.component.{Connector, Logo}
import concussion.facade.ace.AceEditor
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import concussion.facade.draggable._
import react.semanticui.elements.header.Header
import react.semanticui.colors._
import react.semanticui.elements.icon._
import react.semanticui.textalignment._
import react.semanticui.elements.segment._
import scalacss.ProdDefaults._

import scala.scalajs.js
import scala.util.Random

sealed trait Page

object Page {

  case object Landing extends Page
  case object Editor extends Page
  case object NotFound extends Page

  object Style extends StyleSheet.Inline {

    import dsl._

    val programEditor = style(unsafeRoot("#program-editor")(
      height(100.vh),
      width(100.vw),
      display.flex,
      position.relative,
      overflow.auto
    ))
  }

  def landing(r: Random)(ctl: RouterCtl[Page]): VdomElement =
    <.div(
      ^.id := "logo-wrapper",
      ctl.setOnClick(Editor),
      ^.dangerouslySetInnerHtml := Logo(r)
    )

  private val bounds = DraggableBounds(0,null,0,null)

  private val input =
    Draggable(
      Draggable.props(grid = Grid(5,5), handle = ".dragger", bounds = bounds),
      <.div(
        Segment(
          Segment.props(
            className = "dragger",
            inverted = true,
            compact = true,
            attached = SegmentAttached.Top,
            textAlign = Center),
          Header(
            Header.props(as = "h4", inverted = true, color = Green),
            "INPUT"
          )
        ),
        Segment(
          Segment.props(inverted = true, compact = true, attached = SegmentAttached.Bottom, textAlign = Center),
          <.div(
            <.div(^.cls := "ui inverted transparent input",
              <.input(^.`type` := "text", ^.size := 5, ^.defaultValue := "Port1")
            ),
            Icon(Icon.props(name = "dot circle outline"))
          ),
          <.div(
            <.div(^.cls := "ui inverted transparent input",
              <.input(^.`type` := "text", ^.size := 5, ^.defaultValue := "Port2")
            ),
            Icon(Icon.props(name = "dot circle outline"))
          )
        )
      )
    )

  private val output =
    Draggable(
      Draggable.props(grid = Grid(5,5), handle = ".dragger", bounds = bounds),
      <.div(
        Segment(
          Segment.props(
            className = "dragger",
            inverted = true,
            compact = true,
            attached = SegmentAttached.Top,
            textAlign = Center),
          Header(
            Header.props(as = "h4", inverted = true, color = Red),
            "OUTPUT"
          )
        ),
        Segment(
          Segment.props(inverted = true, compact = true, attached = SegmentAttached.Bottom, textAlign = Center),
          <.div(
            Icon(Icon.props(name = "dot circle outline")),
            <.div(^.cls := "ui inverted transparent input",
              <.input(^.`type` := "text", ^.size := 5, ^.defaultValue := "Port1")
            )
          ),
          <.div(
            Icon(Icon.props(name = "dot circle outline")),
            <.div(^.cls := "ui inverted transparent input",
              <.input(^.`type` := "text", ^.size := 5, ^.defaultValue := "Port2")
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

  private val updateCode: AceEditor.OnChange =
    (e: ReactEvent) => Callback(println(e.toString))

  private val updateDrag: Draggable.DraggableEventHandler =
    (mouse, data) => Callback(println(s"${mouse.clientX},${mouse.clientY}; ${data.x},${data.y}"))

  private val processor =
    Draggable(
      Draggable.props(
        grid = Grid(5,5),
        handle = ".dragger",
        bounds = bounds,
        onStop = updateDrag),
      <.div(
        Segment(
          Segment.props(
            className = "dragger",
            inverted = true,
            compact = true,
            attached = SegmentAttached.Top,
            textAlign = Center),
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
              debounceChangePeriod = 500)
          ),
        ),
        Segment(
          Segment.props(inverted = true, compact = true, attached = SegmentAttached.Bottom, textAlign = Center),
          <.div(
            <.div(^.cls := "ui inverted transparent input",
              <.input(^.`type` := "text", ^.size := 5, ^.defaultValue := "Port1")
            ),
            Icon(Icon.props(name = "dot circle outline")),
          ),
          <.div(
            <.div(^.cls := "ui inverted transparent input",
              <.input(^.`type` := "text", ^.size := 5, ^.defaultValue := "Port2")
            ),
            Icon(Icon.props(name = "dot circle outline")),
          )
        )
      )
    )

  def editor: VdomElement = {
    <.div(
      ^.id := "program-editor",
      Connector(200,201,503,501),
      input,
      input,
      processor,
      processor,
      processor,
      output,
      output
    )
  }

  def notFound(r: Random, size: (Int,Int)): VdomElement = {

    def gradient = {
      val c1 = r.nextInt(360)
      val c2 = c1 + r.nextInt(20)

      js.Dictionary("background" -> s"linear-gradient(to right, hsl($c1, 50%, 10%), hsl($c2, 40%, 50%))")
    }

    val rounded = js.Dictionary(
      "border-radius" -> "5px"
    )

    def logo =
      <.div(
        ^.cls := "logo-container",
        ^.style := gradient, ^.style := rounded,
        ^.dangerouslySetInnerHtml := Logo(r)
      )

    val columns = js.Dictionary(
      "grid-template-columns" -> (1 to size._1).map(_ => "auto").mkString(" ")
    )

    <.div(^.id := "logo-grid", ^.style := columns,
      (1 to (size._1*size._2)).map(_ => logo).toTagMod
    )
  }
}
