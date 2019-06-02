package concussion

import info.BuildInfo
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import concussion.component.Logo
import concussion.routes.Page
import concussion.routes.Page._
import japgolly.scalajs.react.extra.router._
import org.scalajs.dom
import scalacss.ProdDefaults._
import scalacss.ScalaCssReact._

import scala.util.Random

object Main extends IOApp {

//  private def layout(router: RouterCtl[Page], page: Resolution[Page]) =
//    <.div(
//      <.h1("CONCUSSION"),
//      <.p(router.baseUrl.value),
//      page.render()
//    )

  private val routerConfig = RouterConfigDsl[Page].buildConfig { dsl =>
    import dsl._

    (trimSlashes
      | staticRoute(root, Landing) ~> renderR(landing(new Random))
      | staticRoute("#1", Landing2) ~> render(landing2(new Random,(4,3)))
      | staticRoute("#notfound", NotFound) ~> render(notFound)
      )
      .notFound(redirectToPage(NotFound)(Redirect.Replace))
      .setTitle(page => s"${BuildInfo.name.capitalize} | $page")
      //.renderWith(layout)
  }

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO(GlobalStyles.addToDocument())
      _ <- IO(Logo.Style.addToDocument())
      router <- IO(Router(BaseUrl.until_#, routerConfig))
      exitCode <- IO(router().renderIntoDOM(dom.document.getElementById(BuildInfo.rootId)))
        .as(ExitCode.Success)
    } yield exitCode
}
