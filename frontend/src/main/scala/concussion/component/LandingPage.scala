package concussion.component

import concussion.routes.Page
import concussion.routes.Page.Editor
import concussion.styles.LogoStyle
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.component.builder.Builder.Step4
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.extra.{EventListener, OnUnmount}
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import org.scalajs.dom.MouseEvent
import scalacss.ScalaCssReact._

import scala.util.Random

object LandingPage {

  final case class Props(ctl: RouterCtl[Page])

  final case class State(logo: String)

  final class Backend() extends OnUnmount {

    def onMouseEnter(id: String)(e: MouseEvent): Callback =
      Callback(println((id, e.target)))

    def render(props: Props, state: State): VdomElement =
      <.div(
        LogoStyle.logoWrapper,
        props.ctl.setOnClick(Editor),
        ^.dangerouslySetInnerHtml := state.logo
      )
  }

  private def addListeners(
      elementIds: List[String],
      builder: Step4[Props, Children.None, State, Backend, UpdateSnapshot.None]
  ) =
    elementIds.foldLeft(builder)(
      (newBuilder, id) =>
        newBuilder.configure(
          EventListener[MouseEvent].install(
            "mouseenter",
            _.backend.onMouseEnter(id),
            _ => dom.document.getElementById(id)
          )
        )
    )

  private def component(random: Random) =
    addListeners(
      Logo.sections.map(_._2),
      ScalaComponent
        .builder[Props]("LandingPage")
        .initialState(State(Logo(random)))
        .renderBackend[Backend]
    ).build

  def apply(random: Random, ctl: RouterCtl[Page]): Unmounted[Props, State, Backend] =
    component(random)(Props(ctl))

}
