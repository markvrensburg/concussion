package concussion.util

import org.scalajs.dom._
import org.scalajs.dom.raw._

import scala.scalajs.js

object Events {

  type BackgroundChangeEvent = String
  val backgroundChangeEvent: String = "background_change"

  def mkBackgroundChangeEvent(msg: BackgroundChangeEvent): Event = {
    val customEventInit = js.Dynamic.literal(detail = msg).asInstanceOf[CustomEventInit]
    new CustomEvent(
      typeArg = backgroundChangeEvent,
      init = customEventInit
    )
  }
}
