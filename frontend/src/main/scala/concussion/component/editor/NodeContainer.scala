package concussion
package component
package editor

import cats.implicits._
import cats.effect.IO
import concussion.compile.Validation
import concussion.facade.draggable.{
  Draggable,
  DraggableBounds,
  DraggableData,
  Grid
}
import concussion.domain._
import concussion.geometry._
import concussion.styles.NodeStyle
import concussion.util.Namer
import japgolly.scalajs.react.Ref.Simple
import japgolly.scalajs.react._
import japgolly.scalajs.react.CatsReact._
import concussion.util.CatsIOReact._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.{MouseEvent, html}
import react.semanticui.colors.{Blue, Green, Grey, Red}
import react.semanticui.elements.header.Header
import react.semanticui.elements.icon.Icon
import react.semanticui.elements.segment.{Segment, SegmentAttached}
import react.semanticui.textalignment.Center
import scalacss.ScalaCssReact._

object NodeContainer {

  final case class State(
    ports: Vector[(String, Simple[html.Element], Orientation, String)] =
      Vector.empty,
    code: Option[String] = Option.empty,
    doUpdate: Boolean = false
  )

  final case class Props(id: String,
                         nodeType: NodeType,
                         namer: Namer[IO],
                         onPortClick: EditPort => Callback,
                         onPortHover: EditPort => Callback,
                         adjustPorts: Vector[EditPort] => Callback,
                         deletePorts: Vector[String] => Callback,
                         cloneNode: Callback,
                         deleteNode: Callback,
                         bringToFront: Callback)

  final class Backend($ : BackendScope[Props, State]) {

    private val getPorts =
      for {
        state <- $.state
        ports <- state.ports
          .map(p => {
            p._2.get
              .map(e => {
                val rect = e.getBoundingClientRect
                val center = (
                  rect.left + ((rect.right - rect.left) / 2),
                  rect.top + ((rect.bottom - rect.top) / 2)
                )
                EditPort(
                  PortMeta(p._1, Anchor(center._1, center._2, p._3)),
                  p._4
                )
              })
              .asCallback
          })
          .sequence
      } yield ports

//    private def cloneNode(nodeId: String): Callback =
//      for {
//        props <- $.props
//        newNode <- Nodes.copyNode(node, props.namer).toCallback
//        _ <- $.modState(state => {
//          state.copy(
//            network = state.network + Graph.vertices(ports.map((_, node)))
//          )
//        })
//      } yield ()

    def updateConnections(force: Boolean = false): Callback =
      for {
        props <- $.props
        state <- $.state
        ports <- getPorts
        _ <- if (force || state.doUpdate)
          props.adjustPorts(ports.mapFilter(identity)) >> $.modState(
            _.copy(doUpdate = false)
          )
        else
          Callback.empty
      } yield ()

    private val addPort =
      for {
        props <- $.props
        id <- props.namer.nextName(s"${props.id}_Port").toCallback
        _ <- $.modState(state => {
          state.copy(
            ports = state.ports :+ (
              (
                id,
                Ref[html.Element],
                if (props.nodeType == Input) Right
                else Left,
                "Port"
              )
            )
          )
        })
      } yield ()

    private def deletePort(portId: String) =
      for {
        props <- $.props
        _ <- $.modState(state => {
          state
            .copy(ports = state.ports.filter(_._1 != portId), doUpdate = true)
        })
        _ <- props.deletePorts(Vector(portId))
      } yield ()

    private val onDelete: Callback =
      for {
        props <- $.props
        state <- $.state
        _ <- props.deletePorts(state.ports.map(_._1))
        _ <- props.deleteNode
      } yield ()

    private val onClone: Callback =
      for {
        props <- $.props
        _ <- props.cloneNode
      } yield ()

    private def shiftPort(portId: String) =
      $.modState(state => {
        val ports = state.ports.map {
          case (id, ref, orientation, name) if id == portId =>
            (id, ref, orientation.swap, name)
          case port => port
        }
        state.copy(ports = ports, doUpdate = true)
      })

    private def changePortName(portId: String)(newName: String) =
      $.modState(state => {
        val ports = state.ports.map {
          case (id, ref, orientation, _) if id == portId =>
            (id, ref, orientation, newName)
          case port => port
        }
        state.copy(ports = ports, doUpdate = true)
      })

    private def onCodeChange(newCode: String) =
      for {
        props <- $.props
        _ <- $.modState { state =>
          println(s"${props.id}: ${Validation.program[Long](newCode)}")
          state.copy(code = Some(newCode), doUpdate = true)
        }
      } yield ()
    //$.modState(_.copy(code = Some(newCode), doUpdate = true))

    private val bounds = DraggableBounds(-169, null, -20, null)

    private val nodeOptions =
      <.div(
        ^.width := "100%",
        ^.display := "flex",
        ^.justifyContent := "center",
        <.div(
          ^.onClick --> onDelete,
          Icon(
            Icon.props(
              name = "trash alternate outline",
              color = Grey,
              link = true
            )
          )
        ),
        <.div(
          ^.onClick --> onClone,
          Icon(Icon.props(name = "clone outline", color = Grey, link = true))
        )
      )

    private def input(props: Props, state: State) =
      Draggable(
        Draggable
          .props(
            grid = Grid(5, 5),
            handle = ".dragger",
            bounds = bounds,
            onMouseDown = (_: MouseEvent) => props.bringToFront,
            onStop = (_: MouseEvent, _: DraggableData) =>
              updateConnections(force = true)
          ),
        <.div(
          NodeStyle.nodePos,
          Segment(
            Segment.props(
              className = "dragger",
              inverted = true,
              compact = true,
              attached = SegmentAttached.Top,
              textAlign = Center
            ),
            Header(
              Header.props(as = "h4", inverted = true, color = Green),
              "INPUT"
            ),
            nodeOptions
          ),
          Segment(
            Segment.props(
              inverted = true,
              compact = true,
              attached = SegmentAttached.Bottom,
              textAlign = Center
            ),
            //Ports
            React.Fragment(
              state.ports.map(
                p =>
                  <.div(
                    ^.key := p._1,
                    PortContainer(
                      p._1,
                      p._4,
                      p._3,
                      p._2,
                      canDelete = false,
                      props.onPortClick,
                      props.onPortHover,
                      deletePort(p._1),
                      shiftPort(p._1),
                      changePortName(p._1)
                    )
                )
              ): _*
            )
          )
        )
      )

    private def output(props: Props, state: State) =
      Draggable(
        Draggable
          .props(
            grid = Grid(5, 5),
            handle = ".dragger",
            bounds = bounds,
            onMouseDown = (_: MouseEvent) => props.bringToFront,
            onStop = (_: MouseEvent, _: DraggableData) =>
              updateConnections(force = true)
          ),
        <.div(
          NodeStyle.nodePos,
          Segment(
            Segment.props(
              className = "dragger",
              inverted = true,
              compact = true,
              attached = SegmentAttached.Top,
              textAlign = Center
            ),
            Header(
              Header.props(as = "h4", inverted = true, color = Red),
              "OUTPUT"
            ),
            nodeOptions
          ),
          Segment(
            Segment.props(
              inverted = true,
              compact = true,
              attached = SegmentAttached.Bottom,
              textAlign = Center
            ),
            //Ports
            React.Fragment(
              state.ports.map(
                p =>
                  <.div(
                    ^.key := p._1,
                    PortContainer(
                      p._1,
                      p._4,
                      p._3,
                      p._2,
                      canDelete = false,
                      props.onPortClick,
                      props.onPortHover,
                      deletePort(p._1),
                      shiftPort(p._1),
                      changePortName(p._1)
                    )
                )
              ): _*
            )
          )
        )
      )

    private def processor(props: Props, state: State) =
      Draggable(
        Draggable
          .props(
            grid = Grid(5, 5),
            handle = ".dragger",
            bounds = bounds,
            onMouseDown = (_: MouseEvent) => props.bringToFront,
            onStop = (_: MouseEvent, _: DraggableData) =>
              updateConnections(force = true)
          ),
        <.div(
          NodeStyle.nodePos,
          Segment(
            Segment.props(
              className = "dragger",
              inverted = true,
              compact = true,
              attached = SegmentAttached.Top,
              textAlign = Center
            ),
            Header(
              Header.props(as = "h4", inverted = true, color = Blue),
              "PROCESSOR"
            ),
            nodeOptions
          ),
          Segment(
            Segment.props(
              inverted = true,
              compact = true,
              attached = SegmentAttached.Attached
            ),
            CodeEditor(onChange = onCodeChange)
          ),
          Segment(
            Segment.props(
              inverted = true,
              compact = true,
              attached = SegmentAttached.Bottom
            ),
            //Ports
            React.Fragment(
              state.ports.map(
                p =>
                  <.div(
                    ^.key := p._1,
                    PortContainer(
                      p._1,
                      p._4,
                      p._3,
                      p._2,
                      canDelete = true,
                      props.onPortClick,
                      props.onPortHover,
                      deletePort(p._1),
                      shiftPort(p._1),
                      changePortName(p._1)
                    )
                )
              ): _*
            ),
            <.div(
              ^.width := "100%",
              ^.display := "flex",
              ^.justifyContent := "center",
              ^.marginTop := ".25rem",
              <.div(
                ^.onClick --> addPort,
                Icon(Icon.props(name = "plus circle", link = true))
              )
            )
          )
        )
      )

    def render(props: Props, state: State): VdomElement = props.nodeType match {
      case Input     => input(props, state)
      case Output    => output(props, state)
      case Processor => processor(props, state)
    }
  }

  private val component =
    ScalaComponent
      .builder[Props]("NodeContainer")
      .initialStateCallbackFromProps(
        props =>
          props.namer
            .nextName(s"${props.id}_Port")
            .toCallback
            .map(
              name =>
                State(
                  ports = Vector(
                    (
                      name,
                      Ref[html.Element],
                      if (props.nodeType == Input) Right
                      else Left,
                      "Port"
                    )
                  )
              )
          )
      )
      .renderBackend[Backend]
      .componentDidUpdate(_.backend.updateConnections())
      .build

  def apply(
    id: String,
    nodeType: NodeType,
    namer: Namer[IO],
    onPortClick: EditPort => Callback = _ => Callback.empty,
    onPortHover: EditPort => Callback = _ => Callback.empty,
    adjustPorts: Vector[EditPort] => Callback = _ => Callback.empty,
    deletePorts: Vector[String] => Callback = _ => Callback.empty,
    cloneNode: Callback = Callback.empty,
    deleteNode: Callback = Callback.empty,
    bringToFront: Callback = Callback.empty
  ): Unmounted[Props, State, Backend] =
    component(
      Props(
        id,
        nodeType,
        namer,
        onPortClick,
        onPortHover,
        adjustPorts,
        deletePorts,
        cloneNode,
        deleteNode,
        bringToFront
      )
    )
}
