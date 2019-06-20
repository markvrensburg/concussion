package concussion

import info.BuildInfo
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import concussion.routes.Page
import concussion.routes.Page._
import concussion.styles.Style
import japgolly.scalajs.react.React
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import scalacss.ProdDefaults._
import scalacss.ScalaCssReact._

import scala.util.Random

object Main extends IOApp {

  private def layout(r: Random, page: Resolution[Page]) = {

    val c1 = r.nextInt(360)
    val c2 = c1 + r.nextInt(20)

    React.Fragment(
      <.div(
        ^.height := "100vh",
        ^.width := "100vw",
        ^.position := "fixed",
        ^.zIndex := "-100",
        ^.background := s"linear-gradient(to right, hsl($c1, 50%, 10%), hsl($c2, 40%, 50%))"
      ),
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
      _ <- Style.styles.values.toList.traverse(style => IO(style.addToDocument()))
      random <- IO(new Random)
      router <- IO(Router(BaseUrl.until_#, routerConfig(random)))
      exitCode <- IO(router().renderIntoDOM(dom.document.getElementById(BuildInfo.rootId)))
        .as(ExitCode.Success)
    } yield exitCode
}
