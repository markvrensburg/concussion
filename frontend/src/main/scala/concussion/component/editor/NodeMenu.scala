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
import react.semanticui.modules.sidebar.SidebarPushable
import react.semanticui.modules.sidebar.SidebarPusher
import react.semanticui.modules.sidebar.SidebarAnimation._
import react.semanticui.modules.sidebar.SidebarWidth._

object NodeMenu {

  final case class Props(
      logo: String,
      namer: Namer[IO],
      addVertices: (EditNode, List[EditPort]) => Callback
  )

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
      SidebarPushable()(
        Sidebar(
          as = As.Menu(Menu(inverted = true, vertical = true)),
          animation = Overlay,
          visible = state.visible,
          width = Thin,
          className = "node-menu"
        )(
          MenuItem()(
            <.div(
              ^.cls := "logo-menu",
              ^.onClick --> onLogoClick,
              ^.dangerouslySetInnerHtml := props.logo
            )
          ),
          MenuItem(as = "a", onClick = addNode(Input))(
            Icon(name = "sitemap"),
            "INPUT"
          ),
          MenuItem(as = "a", onClick = addNode(Output))(
            Icon(name = "sitemap"),
            "OUTPUT"
          ),
          MenuItem(as = "a", onClick = addNode(Processor))(
            Icon(name = "sitemap"),
            "PROCESSOR"
          ),
          MenuItem(as = "a", disabled = true)(
            Icon(name = "sitemap"),
            "MEMORY"
          ),
          MenuItem(as = "a", disabled = true)(
            Icon(name = "sitemap"),
            "SUB-PROCESS"
          )
        ),
        SidebarPusher()(<.div()(children))
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
      namer: Namer[IO],
      addVertices: (EditNode, List[EditPort]) => Callback = (_, _) => Callback.empty,
      children: VdomNode
  ): Unmounted[Props, State, Backend] =
    component(Props(logo, namer, addVertices))(children)
}
