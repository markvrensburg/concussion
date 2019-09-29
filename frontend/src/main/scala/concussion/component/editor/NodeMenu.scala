package concussion
package component
package editor

import cats.effect.IO
import concussion.domain._
import concussion.util.{Namer, Nodes, Ports}
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.{PropsChildren, _}
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.CatsReact._
import concussion.util.CatsIOReact._
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

  final case class Props(logo: String,
                         namer: Namer[IO],
                         addVertices: (EditNode, List[EditPort]) => Callback)

  final case class State(visible: Boolean = true)

  final class Backend($ : BackendScope[Props, State]) {

    private val onLogoClick: Callback =
      $.modState(state => state.copy(visible = !state.visible))

    private def addNode(nodeType: NodeType): Callback =
      for {
        props <- $.props
        node <- Nodes.mkNode(nodeType, props.namer).toCallback
        ports <- Ports.mkPort(node, props.namer).map(List(_)).toCallback
        _ <- props.addVertices(node, ports)
      } yield ()

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
            MenuItem.props(as = "a", onClick = addNode(Input)),
            Icon(Icon.props(name = "sitemap")),
            "INPUT"
          ),
          MenuItem(
            MenuItem.props(as = "a", onClick = addNode(Output)),
            Icon(Icon.props(name = "sitemap")),
            "OUTPUT"
          ),
          MenuItem(
            MenuItem.props(as = "a", onClick = addNode(Processor)),
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

  def apply(logo: String,
            namer: Namer[IO],
            addVertices: (EditNode, List[EditPort]) => Callback = (_, _) =>
              Callback.empty,
            children: VdomNode): Unmounted[Props, State, Backend] =
    component(Props(logo, namer, addVertices))(children)
}
