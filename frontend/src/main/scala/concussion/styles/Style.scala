package concussion
package styles

import scalacss.ProdDefaults._
import enum.Enum
import scalacss.internal.Keyframes

import scala.concurrent.duration._

sealed trait Style extends StyleSheet.Inline

object GlobalStyle extends Style {
  import dsl._

  style(unsafeRoot("*")(boxSizing.borderBox, padding(0.px), margin(0.px)))
}

object AceEditorStyle extends Style {
  import dsl._

  style(unsafeRoot("#ace-editor")(borderRadius(0.5.em)))
}

object DraggableStyle extends Style {
  import dsl._

  style(unsafeRoot(".dragger")(userSelect.none))

  style(unsafeRoot(".react-draggable")(position.absolute, height(0.px)))
}

object LayoutStyle extends Style {
  import dsl._

  val layoutId: String = "page-layout"

  val layout: StyleA =
    style(height(100.vh), width(100.vw), position.fixed, zIndex(-100))
}

object PageStyle extends Style {
  import dsl._

  val editor: StyleA = style(height(100.vh), width(100.vw))
}

object LogoStyle extends Style {

  import dsl._

  val pulse: Keyframes = keyframes(
    0.%% -> style(opacity(0.3), transform := "scale(1.0)"),
    50.%% -> style(opacity(0.6), transform := "scale(1.05)"),
    100.%% -> style(opacity(0.3), transform := "scale(1.0)")
  )

  val logoWrapper: StyleA = style(
    width(100.vw),
    height(100.vh),
    display.flex,
    justifyContent.stretch,
    overflow.hidden
  )

  val logoGrid: StyleA = style(
    unsafeRoot("#logo-grid")(
      display.grid,
      justifyContent.stretch,
      backgroundColor(rgba(0, 0, 0, 0.7)),
      padding(10.px),
      columnGap(10.px),
      rowGap(10.px),
      width(100.vw),
      height(100.vh)
    )
  )

  val logoContainer: StyleA = style(
    unsafeRoot(".logo-container")(height(100.%%), display.flex, overflow.hidden)
  )

  val logoMenu: StyleA = style(
    unsafeRoot(".logo-menu")(display.flex, overflow.hidden)
  )

  val logo: StyleA = style(
    unsafeRoot(".logo")(
      height(80.%%),
      width(80.%%),
      margin.auto,
      animationName(pulse),
      animationDuration(15.seconds),
      animationIterationCount.infinite,
      animationDirection.alternate
    )
  )

  val glow: StyleA = style(unsafeRoot(".glow")(&.hover(filter := "url(#glow)")))
}

object ConnectorStyle extends Style {
  import dsl._

  val connector: StyleA = style(zIndex(100), pointerEvents.none)

  val portHandle: StyleA = style(
    unsafeRoot(".port-socket")(
      marginLeft(0.25.rem).important,
      marginRight(0.25.rem).important
    )
  )

  val connectorHover: StyleA = style(
    unsafeRoot(".connector")(
      svgFill := "none",
      svgStroke.white,
      svgStrokeOpacity(0.5)
    )
  )
}

object GraphStyle extends Style {
  import dsl._

  val nodeEditorId: String = "node-editor"

  val graphEditor: StyleA = style(
    height(100.%%),
    width(100.%%),
    display.flex,
    position.relative,
    overflow.auto
  )
}

object NodeStyle extends Style {
  import dsl._

  val topNodeZIndex: String = "20"

  val bottomNodeZIndex: String = "10"

  val nodePos: StyleA = style(left(170.px), top(20.px))

  val nodeMenu: StyleA = style(unsafeRoot(".node-menu")(opacity(0.9)))
}

object InfobarStyle extends Style {
  import dsl._

  val infobar: StyleA = style(
    zIndex(100),
    position.fixed,
    opacity(0.9),
    top(2.%%),
    left(50.%%),
    transform := "translateX(-50%)"
  )
}

object ToolbarStyle extends Style {
  import dsl._

  val toolbar: StyleA = style(
    zIndex(100),
    position.fixed,
    opacity(0.9),
    bottom(2.%%),
    left(50.%%),
    transform := "translateX(-50%)"
  )
}

object Style {
  val styles: Enum[Style] = Enum.derived[Style]
}
