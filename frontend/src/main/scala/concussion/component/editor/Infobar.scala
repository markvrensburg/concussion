package concussion.component.editor

import concussion.styles.InfobarStyle
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.collections.menu.{Menu, MenuItem}
import react.semanticui.colors.Grey
import react.semanticui.elements.icon.Icon
import scalacss.ScalaCssReact._

object Infobar {

  private val infobar =
    <.div(
      InfobarStyle.infobar,
      Menu(
        Menu.props(compact = true, inverted = true),
        MenuItem(
          MenuItem.props(name = "program_name"),
          Name(defaultValue = "MAIN_PROGRAM")
        ),
        MenuItem(
          MenuItem.props(name = "new"),
          Icon(
            Icon.props(name = "sitemap", color = Grey, link = true)
          )
        ),
        MenuItem(
          MenuItem.props(name = "save"),
          Icon(
            Icon.props(name = "save outline", color = Grey, link = true)
          )
        ),
        MenuItem(
          MenuItem.props(name = "load"),
          Icon(
            Icon.props(name = "folder outline", color = Grey, link = true)
          )
        )
      )
    )

  private val component =
    ScalaComponent
      .builder[Unit]("Info")
      .renderStatic(infobar)
      .build

  def apply() = component()

}
