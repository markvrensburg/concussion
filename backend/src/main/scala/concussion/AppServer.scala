package concussion

import cats.effect._
import concussion.service.StaticFileService
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.GZip
import org.http4s.server.{Router, Server}

import scala.util.Properties.envOrNone

class AppServer[F[_]: ConcurrentEffect: ContextShift: Timer](developmentMode: Boolean) {

  private val ip = "0.0.0.0"
  private val port = 8090

  private val banner =
    """
      | _______  _____  __   _ _______ _     _ _______ _______ _____  _____  __   _
      | |       |     | | \  | |       |     | |______ |______   |   |     | | \  |
      | |_____  |_____| |  \_| |_____  |_____| ______| ______| __|__ |_____| |  \_|
    """.stripMargin.linesIterator.toList

  private def services(blocker: Blocker) =
    Router[F]("/" -> GZip(StaticFileService[F](blocker, developmentMode))).orNotFound

  def serve(blocker: Blocker): Resource[F, Server[F]] =
    for {
      port <- Resource.liftF(Effect[F].delay(envOrNone("PORT").map(_.toInt).getOrElse(port)))
      server <- BlazeServerBuilder[F]
        .bindHttp(port, ip)
        .withHttpApp(services(blocker))
        .withBanner(banner)
        .resource
    } yield server
}

object AppServer {

  def apply[F[_]: ConcurrentEffect: ContextShift: Timer]: AppServer[F] =
    new AppServer[F](false)

  def apply[F[_]: ConcurrentEffect: ContextShift: Timer](developmentMode: Boolean): AppServer[F] =
    new AppServer[F](developmentMode)
}
