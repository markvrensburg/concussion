object Config {

  object Application {
    val name: String = "concussion"
    val organization: String = "za.co.entelect"
    val version: String = "0.0.1-SNAPSHOT"
    val scalaVersion: String = "2.12.9"
  }

  object Backend {
    val mainClass: String = "concussion.Main"
    val developMainClass: String = "concussion.MainDevelop"
    //Have this set to something like "public/"
    val assetPath: String = "public/"
  }

  object Frontend {
    val rootId: String = "root"
  }

  object Ace {
    val keybindingPath: String = "brace/keybinding"
    val modePath: String = "brace/mode"
    val themePath: String = "brace/theme"

    val keybindingRegex: String = "/keybinding-(.*)[.]js"
    val modeRegex: String = "/mode-(.*)[.]js"
    val themeRegex: String = "/theme-(.*)[.]js"
  }
}
