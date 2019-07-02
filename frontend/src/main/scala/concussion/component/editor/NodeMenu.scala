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

import scala.util.Random

object NodeMenu {

  final case class Props(random: Random, addNode: NodeType => Callback)

  final class Backend() {

    def render(props: Props, children: PropsChildren) =
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
            MenuItem.props(),
            <.div(
              ^.cls := "logo-menu",
              ^.dangerouslySetInnerHtml := Logo(props.random)
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
      random: Random,
      addNode: NodeType => Callback = _ => Callback.empty,
      children: VdomNode
  ): Unmounted[Props, Unit, Backend] =
    component(Props(random, addNode))(children)
}
