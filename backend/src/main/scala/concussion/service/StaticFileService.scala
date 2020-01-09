package concussion
package service

import cats.implicits._
import cats.data._
import cats.effect.{Blocker, ContextShift, Effect}
import org.http4s.{HttpRoutes, MediaType, StaticFile}
import org.http4s.dsl.Http4sDsl
import org.http4s.headers._
import scalatags.Text.all._
import info.BuildInfo

class StaticFileService(developmentMode: Boolean) {

  private val keybindingRegex = BuildInfo.aceKeybindingRegex.r
  private val modeRegex = BuildInfo.aceModeRegex.r
  private val themeRegex = BuildInfo.aceThemeRegex.r

  private val assetPath =
    if (BuildInfo.assetPath.isEmpty) None else Some(s"/${BuildInfo.assetPath}")

  private val supportedStaticExtensions =
    Set(".html", ".js", ".css", ".png", ".ico", ".jpg")

  private val index: String =
    html(
      head(
        title := BuildInfo.name,
        base(href := "/"),
        meta(charset := "utf-8"),
        link(rel := "shortcut icon", href := "favicon.ico"),
        link(
          rel := "stylesheet",
          href := s"//cdn.jsdelivr.net/npm/semantic-ui@${BuildInfo.semanticCssVersion}/dist/semantic.min.css"
        )
      ),
      body(
        div(id := BuildInfo.rootId),
        if (developmentMode)
          Seq(
            script(src := s"${BuildInfo.name}-fastopt-library.js"),
            script(src := s"${BuildInfo.name}-fastopt-loader.js"),
            script(src := s"${BuildInfo.name}-fastopt.js")
          )
        else
          script(src := s"${BuildInfo.name}.js")
      )
    ).render

  private def service[F[_]: ContextShift](blocker: Blocker)(implicit F: Effect[F]) = {

    val getMapping: String => F[Option[String]] = pathInfo =>
      F.delay {
        val path = assetPath.getOrElse("/")

        pathInfo match {
          case keybindingRegex(keybinding) =>
            Some(s"$path${BuildInfo.aceKeybindingPath}/$keybinding.js")
          case modeRegex(mode) =>
            Some(s"$path${BuildInfo.aceModePath}/$mode.js")
          case themeRegex(theme) =>
            Some(s"$path${BuildInfo.aceThemePath}/$theme.js")
          case _ => None
        }
      }

    object dsl extends Http4sDsl[F]
    import dsl._

    val indexRoute = Ok(index).map(_.putHeaders(`Content-Type`(MediaType.text.html)))

    HttpRoutes.of[F] {
      case GET -> Root                => indexRoute
      case GET -> Root / "index.html" => indexRoute
      case req @ GET -> Root / asset if supportedStaticExtensions.exists(asset.endsWith) =>
        StaticFile
          .fromResource[F](s"${assetPath.getOrElse("")}$asset", blocker, req.some)
          .orElse(
            OptionT(getMapping(s"/$asset"))
              .flatMap(StaticFile.fromResource[F](_, blocker, req.some))
          )
          .fold(NotFound())(_.pure[F])
          .flatten
    }
  }
}

object StaticFileService {

  def apply[F[_]: Effect: ContextShift](blocker: Blocker, developmentMode: Boolean): HttpRoutes[F] =
    new StaticFileService(developmentMode).service(blocker)
}
