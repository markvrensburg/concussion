package concussion
package styles

import scalacss.ProdDefaults._
import enum.Enum
import scalacss.internal.Keyframes

import scala.concurrent.duration._

sealed trait Style extends StyleSheet.Inline

object GlobalStyle extends Style {
  import dsl._

  style(
    unsafeRoot("*")(
      boxSizing.borderBox,
      padding(0.px),
      margin(0.px)
    )
  )
}

object AceEditorStyle extends Style {
  import dsl._

  style(
    unsafeRoot("#brace-editor")(
      borderRadius(0.5.em)
    )
  )
}

object DraggableStyle extends Style {
  import dsl._

  style(
    unsafeRoot(".dragger")(
      userSelect.none
    )
  )

  style(
    unsafeRoot(".react-draggable")(
      position.absolute,
      height(0.px)
    )
  )
}

object PageStyle extends Style {
  import dsl._

  val editor: StyleA = style(
    height(100.vh),
    width(100.vw)
  )

  val nodeEditor: StyleA = style(
    height(100.%%),
    width(100.%%),
    display.flex,
    position.relative,
    overflow.auto
  )

  val nodePos: StyleA = style(
    left(200.px)
  )

  val portHandle: StyleA = style(
    unsafeRoot(".port-socket, .port-options")(
      marginLeft(0.25.rem).important,
      marginRight(0.25.rem).important
    )
  )
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
    unsafeRoot(".logo-container")(
      height(100.%%),
      display.flex,
      overflow.hidden
    )
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

  val glow: StyleA = style(
    unsafeRoot(".glow")(
      &.hover(
        filter := "url(#glow)"
      )
    )
  )
}

object ConnectorStyle extends Style {
  import dsl._

  val connector: StyleA = style(
    zIndex(100),
    pointerEvents.none
  )

  val connectorHover: StyleA = style(
    unsafeRoot(".connector")(
      svgStrokeOpacity(1)

      //      &.hover(
//        svgStrokeOpacity(1)
//      )
    )
  )
}

object NodeMenuStyle extends Style {
  import dsl._

  val nodeMenu: StyleA = style(
    unsafeRoot(".node-menu")(
      opacity(0.9)
    )
  )
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
