package concussion.component.editor

import cats.effect.IO
import concussion.util.Namer
import concussion.util.CatsReact._
import enum.Enum
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^.{<, VdomElement}

sealed trait NodeType
case object Input extends NodeType
case object Output extends NodeType
case object Processor extends NodeType

object NodeType {
  val nodeTypes: Enum[NodeType] = Enum.derived[NodeType]

  final case class State(ports: Set[String] = Set.empty)

  final case class Props(id: String, namer: Namer[IO])

  final class Backend() {

    def render: VdomElement = <.div()
  }

  private val component =
    ScalaComponent
      .builder[Props]("NodeEditor")
      .initialStateCallbackFromProps(
        props =>
          props.namer.nextName(s"${props.id}_Port").toCallback.map(name => State(ports = Set(name)))
      )
      .renderBackend[Backend]
      .build

  def apply(id: String, namer: Namer[IO]): Unmounted[Props, State, Backend] =
    component(Props(id, namer))
}
