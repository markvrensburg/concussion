package concussion
package component
package editor

import concussion.styles.ToolbarStyle
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.collections.menu.{Menu, MenuIcon, MenuItem}
import react.semanticui.elements.icon.Icon
import scalacss.ScalaCssReact._

object Toolbar {

  private val toolbar =
    <.div(
      ToolbarStyle.toolbar,
      Menu(
        Menu.props(icon = MenuIcon.Labeled, compact = true, inverted = true),
        MenuItem(
          MenuItem.props(name = "play"),
          Icon(Icon.props(name = "play")),
          "Step"
        ),
        MenuItem(
          MenuItem.props(name = "forward"),
          Icon(Icon.props(name = "forward")),
          "Run"
        ),
        MenuItem(
          MenuItem.props(name = "stop"),
          Icon(Icon.props(name = "stop")),
          "Stop"
        )
      )
    )

  private val component =
    ScalaComponent
      .builder[Unit]("Toolbar")
      .renderStatic(toolbar)
      .build

  def apply(): Unmounted[Unit, Unit, Unit] = component()

}
