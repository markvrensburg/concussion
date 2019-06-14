package concussion

import info.BuildInfo
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import concussion.component.{Connector, Logo}
import concussion.facade.ace.AceEditor
import concussion.facade.draggable.Draggable
import concussion.routes.Page
import concussion.routes.Page._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import scalacss.ProdDefaults._
import scalacss.ScalaCssReact._

import scala.scalajs.js
import scala.util.Random

object Main extends IOApp {

  private def layout(r: Random, page: Resolution[Page]) = {

    val layoutStyle = {
      val c1 = r.nextInt(360)
      val c2 = c1 + r.nextInt(20)

      js.Dictionary(
        "height" -> "100vh",
        "width" -> "100vw",
        "position" -> "fixed",
        "z-index" -> "-100",
        "background" -> s"linear-gradient(to right, hsl($c1, 50%, 10%), hsl($c2, 40%, 50%))"
      )
    }

    <.div(
      <.div(^.style := layoutStyle),
      page.render()
    )
  }

  private def routerConfig(r: Random) = RouterConfigDsl[Page].buildConfig { dsl =>
    import dsl._

    (trimSlashes
      | staticRoute(root, Landing) ~> renderR(landing(r))
      | staticRoute("#edit", Editor) ~> render(editor)
      | staticRoute("#notfound", NotFound) ~> render(notFound(r,(4,3)))
      )
      .notFound(redirectToPage(NotFound)(Redirect.Replace))
      .setTitle(page => s"${BuildInfo.name.capitalize} | $page")
      .renderWith((_, page) => layout(r, page))
  }

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO(GlobalStyles.addToDocument())
      _ <- IO(Draggable.Style.addToDocument())
      _ <- IO(AceEditor.Style.addToDocument())
      _ <- IO(Logo.Style.addToDocument())
      _ <- IO(Page.Style.addToDocument())
      _ <- IO(Connector.Style.addToDocument())
      random <- IO(new Random)
      router <- IO(Router(BaseUrl.until_#, routerConfig(random)))
      exitCode <- IO(router().renderIntoDOM(dom.document.getElementById(BuildInfo.rootId)))
        .as(ExitCode.Success)
    } yield exitCode
}
