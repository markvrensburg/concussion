package concussion
package routes

import concussion.component.Logo
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.MouseEvent
import react.draggable._

import scala.util.Random

sealed trait Page

object Page {

  case object Landing extends Page
  case object NotFound extends Page

  val landing: VdomElement =
    <.div(^.id := "logo-container",
      ^.dangerouslySetInnerHtml := Logo(new Random)
    )

  val notFound: VdomElement =
    <.div(
      Draggable(Draggable.props(
        //axis = Axis.Both,
        //defaultClassName = "DragHandle",
        //defaultClassNameDragging = "DragHandleActive",
        onDrag = (ev: MouseEvent, d: DraggableData) => Callback(println((ev,d.node))),
        //position = ControlPosition(0)
        ),
        <.div(
          <.h2("NOT_FOUND")
        )
      )
    )
}
