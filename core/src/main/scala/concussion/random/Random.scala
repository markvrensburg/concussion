package concussion
package random

import scala.language.higherKinds

trait Random[F[_]] {
  def long: F[Long]
}
