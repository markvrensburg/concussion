package concussion
package component
package editor

import japgolly.scalajs.react.{PropsChildren, _}
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.VdomNode
import react.semanticui._
import react.semanticui.collections.menu.MenuItem
import react.semanticui.elements.icon.Icon
import react.semanticui.collections.menu.Menu
import react.semanticui.modules.sidebar.Sidebar
import react.semanticui.modules.sidebar.Sidebar.Pushable
import react.semanticui.modules.sidebar.Sidebar.Pusher
import react.semanticui.modules.sidebar.SidebarAnimation._
import react.semanticui.modules.sidebar.SidebarWidth._

object NodeMenu {

  class Backend() {

  def render(C: PropsChildren) =
    Sidebar.Pushable(
      Pushable.props(),
      Sidebar(
        Sidebar.props(
          as = As.Menu(Menu.props(inverted = true, vertical = true)),
          animation = Overlay,
          visible = true,
          width = Thin,
          className = "node-menu"
        ),
        MenuItem(
          MenuItem.props(as = "a"),
          Icon(Icon.props(name = "home")),
          "HOME"
        )
      ),
      Sidebar.Pusher(
        Pusher.props(),
        <.div()(C)
      )
    )

  }

  private val component = ScalaComponent.builder[Unit]("NodeMenu")
    .renderBackendWithChildren[Backend]
    .build


  def apply(children: VdomNode) =
    component(children)
}
