package concussion

import cats.implicits._
import cats.effect._

object MainDevelop extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    Blocker[IO]
      .flatMap(blocker => AppServer[IO](developmentMode = true).serve(blocker))
      .use(_ => IO.never)
      .as(ExitCode.Success)
}
