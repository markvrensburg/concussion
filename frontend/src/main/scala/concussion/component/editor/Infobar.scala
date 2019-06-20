package concussion.component.editor

import concussion.styles.InfobarStyle
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.collections.menu.{Menu, MenuItem}
import scalacss.ScalaCssReact._

object Infobar {

  private val infobar =
    <.div(
      InfobarStyle.infobar,
      Menu(
        Menu.props(compact = true, inverted = true),
        MenuItem(
          MenuItem.props(name = "info"),
          Input(defaultValue = "Main")
        )
      )
    )

  private val component = ScalaComponent.builder[Unit]("Info")
    .renderStatic(infobar)
    .build

  def apply() = component()

}