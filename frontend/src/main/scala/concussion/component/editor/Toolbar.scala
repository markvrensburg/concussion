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
      Menu(icon = MenuIcon.Labeled, compact = true, inverted = true)(
        MenuItem(name = "play")(Icon(name = "play", link = true), "Step"),
        MenuItem(name = "forward")(Icon(name = "forward", link = true), "Run"),
        MenuItem(name = "stop")(Icon(name = "stop", link = true), "Stop")
      )
    )

  private val component =
    ScalaComponent
      .builder[Unit]("Toolbar")
      .renderStatic(toolbar)
      .build

  def apply(): Unmounted[Unit, Unit, Unit] = component()

}
