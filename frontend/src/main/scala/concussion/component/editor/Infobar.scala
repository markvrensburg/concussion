package concussion
package component
package editor

import concussion.styles.InfobarStyle
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.collections.menu.{Menu, MenuItem}
import react.semanticui.colors.Grey
import react.semanticui.elements.icon.Icon
import scalacss.ScalaCssReact._

object Infobar {

  private val infobar =
    <.div(
      InfobarStyle.infobar,
      Menu(compact = true, inverted = true)(
        MenuItem(name = "program_name")(Name(defaultValue = "MAIN_PROGRAM")),
        MenuItem(name = "new")(Icon(name = "file outline", color = Grey, link = true)),
        MenuItem(name = "save")(Icon(name = "save outline", color = Grey, link = true)),
        MenuItem(name = "load")(Icon(name = "folder outline", color = Grey, link = true))
      )
    )

  private val component =
    ScalaComponent
      .builder[Unit]("Info")
      .renderStatic(infobar)
      .build

  def apply(): Unmounted[Unit, Unit, Unit] = component()

}
