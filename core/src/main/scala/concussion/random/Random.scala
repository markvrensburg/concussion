package concussion
package random

trait Random[F[_]] {
  def long: F[Long]
}
