object Config {

  object Application {
    val name: String = "concussion"
    val organization: String = "za.co.entelect"
    val version: String = "0.0.1-SNAPSHOT"
    val scalaVersion: String = "2.13.1"
  }

  object Backend {
    val mainClass: String = "concussion.Main"
    val developMainClass: String = "concussion.MainDevelop"
    val assetPath: String = "public/" //Have this set to something like "public/"
  }

  object Frontend {
    val rootId: String = "root"
  }

  object Ace {
    val sourcePath: String = "ace-builds/src-min-noconflict"

    val keybindingRegex: String = "/(keybinding-.*)[.]js"
    val modeRegex: String = "/(mode-.*)[.]js"
    val themeRegex: String = "/(theme-.*)[.]js"
  }
}
