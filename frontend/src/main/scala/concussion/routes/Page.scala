package concussion
package routes

import cats.effect.IO
import concussion.component._
import concussion.component.editor._
import concussion.util.Namer
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._

import scala.scalajs.js
import scala.util.Random

sealed trait Page

object Page {

  case object Landing extends Page
  case object Editor extends Page
  case object NotFound extends Page

  def landing(random: Random)(ctl: RouterCtl[Page]): VdomElement =
    LandingPage(random, ctl)

  def editor(random: Random, namer: Namer[IO]): VdomElement =
    GraphEditor(random, namer)

  def notFound(r: Random, size: (Int, Int)): VdomElement = {

    def logo =
      <.div(
        ^.cls := "logo-container",
        ^.background := Background(r),
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
