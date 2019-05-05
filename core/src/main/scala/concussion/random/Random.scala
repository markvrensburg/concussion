package concussion
package random

case class Range[A: Numeric](min: A, max: A)

trait Random[F[_],RNG] {
  def nextInt(range: Range[Int] = Range(Int.MinValue, Int.MaxValue)): F[Int]
  def nextLong(range: Range[Long] = Range(Long.MinValue, Long.MaxValue)): F[Long]
  def nextDouble(range: Range[Double] = Range(Double.MinValue, Double.MaxValue)): F[Double]
}
