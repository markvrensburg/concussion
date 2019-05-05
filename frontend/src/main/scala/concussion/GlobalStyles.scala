package concussion

import info.BuildInfo
import scalacss.ProdDefaults._

object GlobalStyles extends StyleSheet.Inline {
  import dsl._

  style(unsafeRoot("html")(
    height(100.%%)
  ))

  style(unsafeRoot("body")(
    height(100.%%),
    margin(0.px),
  ))

  style(unsafeRoot(s"#${BuildInfo.rootId}")(
    height(100.%%)
  ))
}