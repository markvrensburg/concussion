package concussion

import cats.effect.concurrent.Ref
import info.BuildInfo
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import concussion.component.Background
import concussion.routes.Page
import concussion.routes.Page._
import concussion.styles.Style
import concussion.util.Namer
import japgolly.scalajs.react.React
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import scalacss.ProdDefaults._
import scalacss.ScalaCssReact._

import scala.util.Random

object Main extends IOApp {

  private def layout(background: String, page: Resolution[Page]) =
    React.Fragment(
      <.div(
        ^.height := "100vh",
        ^.width := "100vw",
        ^.position := "fixed",
        ^.zIndex := "-100",
        ^.background := background
      ),
      page.render()
    )

  private def routerConfig(background: Ref[IO, String], random: Random, namer: Namer[IO]) =
    RouterConfigDsl[Page].buildConfig { dsl =>
      import dsl._
      (trimSlashes
        | staticRoute(root, Landing) ~> renderR(landing(random))
        | staticRoute("#edit", Editor) ~> render(editor(random, namer))
        | staticRoute("#notfound", NotFound) ~> render(notFound(random, (4, 3))))
        .notFound(redirectToPage(NotFound)(Redirect.Replace))
        .setTitle(page => s"${BuildInfo.name.capitalize} | $page")
        .renderWith((_, page) => layout(background.get.unsafeRunSync, page))
    }

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- Style.styles.values.toList.traverse(style => IO(style.addToDocument()))
      random <- IO(new Random)
      namer <- Namer[IO]
      background <- Ref[IO].of(Background(random))
      router <- IO(Router(BaseUrl.until_#, routerConfig(background, random, namer)))
      exitCode <- IO(router().renderIntoDOM(dom.document.getElementById(BuildInfo.rootId)))
        .as(ExitCode.Success)
    } yield exitCode
}
