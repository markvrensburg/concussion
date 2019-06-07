package concussion
package routes

import concussion.component.Logo
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import concussion.facade.draggable._
//import concussion.semantic.SemanticUiCss
import react.semanticui.colors._
import react.semanticui.elements.icon._
import react.semanticui.elements.label.{Label, LabelDetail}
import react.semanticui.elements.segment._

import scala.scalajs.js
import scala.util.Random

sealed trait Page

object Page {

  case object Landing extends Page
  case object Landing2 extends Page //@todo remove
  case object NotFound extends Page

  //val semanticUiCss = SemanticUiCss

  private def gradient(r: Random) = {
    val c1 = r.nextInt(360)
    val c2 = c1 + r.nextInt(20)

    js.Dictionary("background" -> s"linear-gradient(to right, hsl($c1, 50%, 10%), hsl($c2, 40%, 50%))")
  }

  def landing(r: Random)(ctl: RouterCtl[Page]): VdomElement =
    <.div(
      ^.id := "logo-wrapper",
      ^.style := gradient(r),
      ctl.setOnClick(Landing2),
      ^.dangerouslySetInnerHtml := Logo(r)
    )

  def landing2(r: Random, size: (Int,Int)): VdomElement = {

    val rounded = js.Dictionary(
      "border-radius" -> "5px"
    )

    def logo =
      <.div(
        ^.cls := "logo-container",
        ^.style := gradient(r), ^.style := rounded,
        ^.dangerouslySetInnerHtml := Logo(r)
      )

    val columns = js.Dictionary(
      "grid-template-columns" -> (1 to size._1).map(_ => "auto").mkString(" ")
    )

    <.div(^.id := "logo-grid", ^.style := columns,
      (1 to (size._1*size._2)).map(_ => Draggable(
        Draggable.props(bounds = "body", grid = Grid(5,5)),
        logo
      )).toTagMod
    )
  }

  val notFound: VdomElement =
    Draggable(
      Draggable.props(bounds = "body", grid = Grid(5,5)),
      Segment(
        Segment.props(inverted = true, compact = true),
        Label(
          Label.props(color = Orange),
          Icon(Icon.props(name = "check")),
          "ACC",
          LabelDetail(
            LabelDetail.props(),
            42
          )
        )
      )
    )
}
