package concussion
package service

import cats.implicits._
import cats.data._
import cats.effect.{ContextShift, Effect}
import org.http4s.{HttpRoutes, MediaType, StaticFile}
import org.http4s.dsl.Http4sDsl
import org.http4s.headers._
import java.util.concurrent._
import scalatags.Text.all._
import scala.concurrent.ExecutionContext
import info.BuildInfo

class StaticFileService(developmentMode: Boolean) {

  private val keybindingRegex = BuildInfo.aceKeybindingRegex.r
  private val modeRegex = BuildInfo.aceModeRegex.r
  private val themeRegex = BuildInfo.aceThemeRegex.r

  private val assetPath = if (BuildInfo.assetPath.isEmpty) None else Some(s"/${BuildInfo.assetPath}")

  private val supportedStaticExtensions = Set(
    ".html",
    ".js",
    ".css",
    ".png",
    ".ico",
    ".jpg"
  )

  private val blockingEc =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors))

  private val index: String =
    html(
      head(
        title := BuildInfo.name,
        base(href := "/"),
        meta(charset := "utf-8"),
        link(rel := "shortcut icon", href := "favicon.ico")
      ),
      body(
        div(id := BuildInfo.rootId),
        if (developmentMode)
          script(src := s"${BuildInfo.name}.js")
        else
          Seq(
            script(src := s"${BuildInfo.name}-fastopt-library.js"),
            script(src := s"${BuildInfo.name}-fastopt-loader.js"),
            script(src := s"${BuildInfo.name}-fastopt.js")
          )
      )
  ).render

  private def service[F[_]: ContextShift](implicit F: Effect[F]) = {

    val getAceMapping: String => F[Option[String]] = pathInfo => F.delay {
      val path = assetPath.getOrElse("/")

      pathInfo match {
        case keybindingRegex(keybinding) => Some(s"$path${BuildInfo.aceKeybindingPath}/$keybinding.js")
        case modeRegex(mode) => Some(s"$path${BuildInfo.aceModePath}/$mode.js")
        case themeRegex(theme) => Some(s"$path${BuildInfo.aceThemePath}/$theme.js")
        case _ => None
      }
    }

    object dsl extends Http4sDsl[F]
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root =>
        Ok(index).map(_.putHeaders(`Content-Type`(MediaType.text.html)))
      case GET -> Root / "index.html" =>
        Ok(index).map(_.putHeaders(`Content-Type`(MediaType.text.html)))
      case req@GET -> Root / "favicon.ico" =>
        StaticFile.fromResource[F](s"${assetPath.getOrElse("")}favicon.ico", blockingEc, req.some)
          .fold(NotFound())(_.pure[F])
          .flatten
      case req@GET -> Root / asset if supportedStaticExtensions.exists(asset.endsWith) =>
        StaticFile.fromResource[F](s"${assetPath.getOrElse("")}$asset", blockingEc, req.some)
          .orElse(OptionT(getAceMapping(s"/$asset")).flatMap(StaticFile.fromResource[F](_, blockingEc, req.some)))
          .fold(NotFound())(_.pure[F])
          .flatten
    }
  }
}

object StaticFileService {

  def apply[F[_]: Effect: ContextShift](developmentMode: Boolean): HttpRoutes[F] =
    new StaticFileService(developmentMode).service
}
