package concussion
package routes

import concussion.component.Logo
import japgolly.scalajs.react.vdom.html_<^._

import scala.util.Random

sealed trait Page

object Page {

  case object Landing extends Page
  case object NotFound extends Page

  val landing: VdomElement =
    <.div(^.id := "logo-container",
      ^.dangerouslySetInnerHtml := Logo(new Random)
    )

  val notFound: VdomElement =
    <.div(
      <.h2("NOT_FOUND")
    )

}
