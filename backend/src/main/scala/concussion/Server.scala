package concussion

import cats.effect._
import cats.implicits._
import concussion.service.StaticFileService
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.GZip
import org.http4s.server.Router

import scala.util.Properties.envOrNone

class Server[F[_]: ConcurrentEffect: ContextShift: Timer](developmentMode: Boolean) {

  private val ip = "0.0.0.0"
  private val port = 8090

  private val banner =
    """
      | _______  _____  __   _ _______ _     _ _______ _______ _____  _____  __   _
      | |       |     | | \  | |       |     | |______ |______   |   |     | | \  |
      | |_____  |_____| |  \_| |_____  |_____| ______| ______| __|__ |_____| |  \_|
    """.stripMargin.lines.toList

  private val services =
    Router[F]("/" -> GZip(StaticFileService[F](developmentMode))).orNotFound

  def serve: F[ExitCode] =
    for {
      port <- Effect[F].delay(envOrNone("PORT").map(_.toInt).getOrElse(port))
      exitCode <- BlazeServerBuilder[F]
        .bindHttp(port, ip)
        .withHttpApp(services)
        .withBanner(banner)
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    } yield exitCode
}

object Server {

  def apply[F[_]: ConcurrentEffect: ContextShift: Timer]: Server[F] =
    new Server[F](false)

  def apply[F[_]: ConcurrentEffect: ContextShift: Timer](developmentMode: Boolean): Server[F] =
    new Server[F](developmentMode)
}
