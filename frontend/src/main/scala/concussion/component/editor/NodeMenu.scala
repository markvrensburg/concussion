package concussion
package component
package editor

import japgolly.scalajs.react.component.Scala.Unmounted
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

  final case class Props(
      logo: String,
      visible: Boolean,
      addNode: NodeType => Callback,
      onLogoClick: Callback
  )

  final class Backend() {

    def render(props: Props, children: PropsChildren) =
      Sidebar.Pushable(
        Pushable.props(),
        Sidebar(
          Sidebar.props(
            as = As.Menu(Menu.props(inverted = true, vertical = true)),
            animation = Overlay,
            visible = props.visible,
            width = Thin,
            className = "node-menu"
          ),
          MenuItem(
            MenuItem.props(),
            <.div(
              ^.cls := "logo-menu",
              ^.onClick --> props.onLogoClick,
              ^.dangerouslySetInnerHtml := props.logo
            )
          ),
          MenuItem(
            MenuItem.props(as = "a", onClick = props.addNode(Input)),
            Icon(Icon.props(name = "sitemap")),
            "INPUT"
          ),
          MenuItem(
            MenuItem.props(as = "a", onClick = props.addNode(Output)),
            Icon(Icon.props(name = "sitemap")),
            "OUTPUT"
          ),
          MenuItem(
            MenuItem.props(as = "a", onClick = props.addNode(Processor)),
            Icon(Icon.props(name = "sitemap")),
            "PROCESSOR"
          ),
          MenuItem(
            MenuItem.props(as = "a", disabled = true),
            Icon(Icon.props(name = "sitemap")),
            "MEMORY"
          ),
          MenuItem(
            MenuItem.props(as = "a", disabled = true),
            Icon(Icon.props(name = "sitemap")),
            "SUB-PROCESS"
          )
        ),
        Sidebar.Pusher(
          Pusher.props(),
          <.div()(children)
        )
      )
  }

  private val component =
    ScalaComponent
      .builder[Props]("NodeMenu")
      .renderBackendWithChildren[Backend]
      .build

  def apply(
      logo: String,
      visible: Boolean = true,
      addNode: NodeType => Callback = _ => Callback.empty,
      onLogoClick: Callback = Callback.empty,
      children: VdomNode
  ): Unmounted[Props, Unit, Backend] =
    component(Props(logo, visible, addNode, onLogoClick))(children)
}
