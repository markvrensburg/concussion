package concussion
package routes

import concussion.component._
import concussion.component.editor._
import concussion.styles.LogoStyle
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
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
      LogoStyle.logoWrapper,
      ctl.setOnClick(Editor),
      ^.dangerouslySetInnerHtml := Logo(r)
    )

  def editor: VdomElement =
    NodeEditor()

  def notFound(r: Random, size: (Int, Int)): VdomElement = {

    def background = {
      val c1 = r.nextInt(360)
      val c2 = c1 + r.nextInt(20)
      s"linear-gradient(to right, hsl($c1, 50%, 10%), hsl($c2, 40%, 50%))"
    }

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
