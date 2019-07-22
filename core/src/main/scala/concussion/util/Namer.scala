package concussion
package util

import cats.implicits._
import cats.effect.Sync
import cats.effect.concurrent.Ref

trait Namer[F[_]] {
  def nextName(prefix: String): F[String]
}

object Namer {

  def apply[F[_]: Sync]: F[Namer[F]] =
    Ref[F].of(Map.empty[String, Int]).map { nameCounter => (prefix: String) =>
      nameCounter.modify { oldMap =>
        val n = oldMap.getOrElse(prefix, 0)
        (oldMap + (prefix -> (n + 1)), s"${prefix}_$n")
      }
    }

}
