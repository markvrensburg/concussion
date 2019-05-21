package concussion

import concussion.service.StaticFileService
import cats.implicits._
import cats.effect._
import org.http4s.server.blaze._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.middleware.GZip

import scala.util.Properties.envOrNone

object Main extends IOApp {

  private val ip = "0.0.0.0"
  private val port = 8090

  private val banner = """
                         | _______  _____  __   _ _______ _     _ _______ _______ _____  _____  __   _
                         | |       |     | | \  | |       |     | |______ |______   |   |     | | \  |
                         | |_____  |_____| |  \_| |_____  |_____| ______| ______| __|__ |_____| |  \_|
                         """.stripMargin.lines.toList

  private val services = Router(
    "/" -> GZip(StaticFileService[IO])
  ).orNotFound

  def run(args: List[String]): IO[ExitCode] =
    for {
      port <- IO(envOrNone("PORT").map(_.toInt).getOrElse(port))
      exitCode <- BlazeServerBuilder[IO]
        .bindHttp(port, ip)
        .withHttpApp(services)
        .withBanner(banner)
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    } yield exitCode
}
