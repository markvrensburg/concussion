package concussion.util

import org.scalajs.dom._
import org.scalajs.dom.raw._

object Events {

  val backgroundChangeEvent: String = "backgroundchange"

  def mkBackgroundChangeEvent: Event = {
    val event = document.createEvent("CustomEvent").asInstanceOf[CustomEvent]
    event.initCustomEvent(backgroundChangeEvent, false, false, null)
    event
  }

}
