package concussion
package facade

import scala.scalajs.js
import scala.scalajs.js.|
import japgolly.scalajs.react.raw.JsNumber
import org.scalajs.dom.html.{Element => HTMLElement}
import org.scalajs.dom.MouseEvent
import concussion.facade.common.EnumValue

package object draggable {
  type Bounds = DraggableBounds | String | FalseBounds.type
}

package draggable {
  sealed trait Axis

  object Axis {
    implicit val enumValue: EnumValue[Axis] = EnumValue.toLowerCaseString

    case object Both extends Axis
    case object X extends Axis
    case object Y extends Axis
    case object None extends Axis
  }

  case class Grid(x: JsNumber, y: JsNumber) {
    val value: js.Array[JsNumber] = js.Array(x, y)
  }

  @js.native
  trait ControlPosition extends js.Object {
    var x: js.UndefOr[JsNumber]
    var y: js.UndefOr[JsNumber]
  }

  object ControlPosition {

    def apply(
        x: js.UndefOr[JsNumber] = js.undefined,
        y: js.UndefOr[JsNumber] = js.undefined
    ): ControlPosition = {
      val p = (new js.Object).asInstanceOf[ControlPosition]
      p.x = x
      p.y = y
      p
    }
  }

  @js.native
  trait DraggableBounds extends js.Object {
    var left: JsNumber
    var right: JsNumber
    var top: JsNumber
    var bottom: JsNumber
  }

  object DraggableBounds {

    def apply(left: JsNumber, right: JsNumber, top: JsNumber, bottom: JsNumber): DraggableBounds = {
      val p = (new js.Object).asInstanceOf[DraggableBounds]
      p.left = left
      p.right = right
      p.top = top
      p.bottom = bottom
      p
    }
  }

  @js.native
  trait DraggableData extends js.Object {
    def node: HTMLElement = js.native
    def x: JsNumber = js.native
    def y: JsNumber = js.native
    def deltaX: JsNumber = js.native
    def deltaY: JsNumber = js.native
    def lastX: JsNumber = js.native
    def lastY: JsNumber = js.native
  }

  @js.native
  trait PositionOffsetControlPosition extends js.Object {
    var x: js.UndefOr[String | JsNumber]
    var y: js.UndefOr[String | JsNumber]
  }

  object PositionOffsetControlPosition {

    def apply(
        x: js.UndefOr[String | JsNumber] = js.undefined,
        y: js.UndefOr[String | JsNumber] = js.undefined
    ): PositionOffsetControlPosition = {
      val p = (new js.Object).asInstanceOf[PositionOffsetControlPosition]
      p.x = x
      p.y = y
      p
    }
  }

  case object FalseBounds

  private[draggable] object rawjs {
    type RawOnMouseDown = js.Function1[MouseEvent, Unit]

    type RawDraggableEventHandler =
      js.Function2[MouseEvent, DraggableData, Unit]
  }
}
