package concussion.component

import scala.util.Random

object Background {

  def apply(r: Random): String = {
    val c1 = r.nextInt(360)
    val c2 = c1 + r.nextInt(20)
    s"linear-gradient(to right, hsl($c1, 50%, 10%), hsl($c2, 40%, 50%))"
  }
}
