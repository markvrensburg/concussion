package concussion

import info.BuildInfo
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import japgolly.scalajs.react.CatsReact._
import concussion.util.CatsIOReact._
import concussion.component.{Background, Logo}
import concussion.routes.Page
import concussion.routes.Page._
import concussion.styles.Style
import concussion.util.Namer
import japgolly.scalajs.react.extra.router._
import org.scalajs.dom
import scalacss.ProdDefaults._
import scalacss.ScalaCssReact._

import scala.util.Random

object Main extends IOApp {

  private def routerConfig(random: Random, namer: Namer[IO]) =
    RouterConfigDsl[Page].buildConfig { dsl =>
      import dsl._
      (trimSlashes
        | staticRoute(root, Landing) ~> renderR(landing(random))
        | staticRoute("#edit", Editor) ~> render(editor(Logo(random), namer))
        | staticRoute("#notfound", NotFound) ~> render(
          notFound(random, (4, 3))
        ))
        .notFound(redirectToPage(NotFound)(SetRouteVia.HistoryReplace))
        .setTitle(page => s"${BuildInfo.name.capitalize} | $page")
        .renderWith(
          (_, page) => Layout(IO.delay(Background(random)).toCallback)(page.render())
        )
    }

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- Style.styles.values.toList
        .traverse(style => IO(style.addToDocument()))
      random <- IO(new Random)
      namer <- Namer[IO]
      router <- IO(Router(BaseUrl.until_#, routerConfig(random, namer)))
      exitCode <- IO(
        router().renderIntoDOM(dom.document.getElementById(BuildInfo.rootId))
      ).as(ExitCode.Success)
    } yield exitCode
}
