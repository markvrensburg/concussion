package concussion.component.editor

import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.{document, html}
import react.semanticui.elements.icon.Icon

object PortSocket {

  final class Backend() {

    private val portSocketRef = Ref[html.Element]

    private val logLocation =
      portSocketRef.foreach(e => {
        val editor = document.getElementById("node-editor")
        val rect = e.getBoundingClientRect
        println(
          s"top: ${rect.top}, left: ${rect.left}, scroll: (${editor.scrollTop},${editor.scrollLeft})"
        )
      })

    private val portSocket =
      <.div(
        ^.onMouseDown ==> { _ =>
          logLocation
        },
        ^.onMouseUp ==> { _ =>
          logLocation
        },
        Icon(Icon.props(name = "dot circle outline", className = "port-socket"))
      )

    def render: VdomElement =
      portSocket.withRef(portSocketRef)

  }

  private val component = ScalaComponent
    .builder[Unit]("PortContainer")
    .renderBackend[Backend]
    .build

  def apply(): Unmounted[Unit, Unit, Backend] = component()

}
