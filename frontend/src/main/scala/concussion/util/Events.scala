package concussion.util

import org.scalajs.dom._
import org.scalajs.dom.raw._

object Events {

  val backgroundChangeEvent: String = "backgroundchange"

  def mkBackgroundChangeEvent: Event =
    new CustomEvent(
      typeArg = backgroundChangeEvent,
      init = new CustomEventInit {}
    )
}
