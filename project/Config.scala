object Config {

  object Application {
    val name: String = "concussion"
    val organization: String = "za.co.entelect"
    val version: String = "0.0.1-SNAPSHOT"
    val scalaVersion: String = "2.12.8"
  }

  object Backend {
    val mainClass: String = "concussion.Main"
    //Have this set to something like "public/"
    val assetPath: String = "public/"
  }

  object Frontend {
    val rootId: String = "root"
  }

  object Ace {
    val keybindingPath: String = s"brace/keybinding"
    val modePath: String = s"brace/mode"
    val themePath: String = s"brace/theme"

    val keybindingRegex: String = "/keybinding-(.*)[.]js"
    val modeRegex: String = "/mode-(.*)[.]js"
    val themeRegex: String = "/theme-(.*)[.]js"
  }
}
