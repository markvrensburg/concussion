package concussion
package component
package editor

import concussion.domain._
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

  final case class Props(logo: String, addNode: NodeType => Callback)

  final case class State(visible: Boolean = true)

  final class Backend($ : BackendScope[Props, State]) {

    private val onLogoClick: Callback =
      $.modState(state => {
        state //todo uncomment: state.copy(visible = !state.visible)
      })

    def render(props: Props, state: State, children: PropsChildren) =
      Sidebar.Pushable(
        Pushable.props(),
        Sidebar(
          Sidebar.props(
            as = As.Menu(Menu.props(inverted = true, vertical = true)),
            animation = Overlay,
            visible = state.visible,
            width = Thin,
            className = "node-menu"
          ),
          MenuItem(
            MenuItem.props(),
            <.div(
              ^.cls := "logo-menu",
              ^.onClick --> onLogoClick,
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
        Sidebar.Pusher(Pusher.props(), <.div()(children))
      )
  }

  private val component =
    ScalaComponent
      .builder[Props]("NodeMenu")
      .initialState(State())
      .renderBackendWithChildren[Backend]
      .build

  def apply(
      logo: String,
      addNode: NodeType => Callback = _ => Callback.empty,
      children: VdomNode
  ): Unmounted[Props, State, Backend] =
    component(Props(logo, addNode))(children)
}
