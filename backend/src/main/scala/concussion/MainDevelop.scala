package concussion

import cats.effect._

object MainDevelop extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    Server[IO](developmentMode = false).serve
}
