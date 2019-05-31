package concussion

import scalacss.ProdDefaults._

object GlobalStyles extends StyleSheet.Inline {
  import dsl._

  style(unsafeRoot("*")(
    boxSizing.borderBox,
    padding(0.px),
    margin(0.px)
  ))
}