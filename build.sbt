import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import scalajsbundler.sbtplugin.WebScalaJSBundlerPlugin.autoImport.npmAssets
import Config._
import Dependencies._
import Environment._

//addCommandAlias("root", ";project root")
//addCommandAlias("core", ";project coreJVM")
//addCommandAlias("frontend", ";project frontend")
//addCommandAlias("backend", ";project backend")

lazy val commonSettings = Seq(
  name := Application.name,
  organization := Application.organization,
  version := Application.version,
  scalaVersion := Application.scalaVersion,
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.10"),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0")
)

lazy val root = project.in(file("."))
  .settings(commonSettings)
  .settings(
    publish := {},
    publishLocal := {}
  )
  .aggregate(frontend, backend)

lazy val core = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("core"))
    .enablePlugins(BuildInfoPlugin)
    .settings(commonSettings)
    .settings(
      buildInfoOptions ++= Seq(
        BuildInfoOption.BuildTime,
        BuildInfoOption.ToJson
      ),
      buildInfoKeys := Seq[BuildInfoKey](
        name,
        version,
        "assetPath" -> Backend.assetPath,
        "rootId" -> Frontend.rootId,
        "aceKeybindingPath" -> Ace.keybindingPath,
        "aceModePath" -> Ace.modePath,
        "aceThemePath" -> Ace.themePath,
        "aceKeybindingRegex" -> Ace.keybindingRegex,
        "aceModeRegex" -> Ace.modeRegex,
        "aceThemeRegex" -> Ace.themeRegex,
        "semanticCssVersion" -> semanticUICssV
      ),
      buildInfoPackage := "info",
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core" % catsV,
        "org.typelevel" %%% "cats-effect" % catsEffectV,
        "org.tpolecat" %%% "atto-core" % attoV,
        "co.fs2" %%% "fs2-core" % fs2V,
        "com.lihaoyi" %%% "scalatags" % scalaTagsV,
        "org.julienrf" %%% "enum" % enumV,
        "org.scalacheck" %%% "scalacheck" % "1.14.0" % "test",
        "org.scalatest" %%% "scalatest" % "3.0.5" % "test"
      )
    )
    .jvmSettings(
      test in assembly := {}
    )
    .jsSettings(
    )

lazy val coreJvm = core.jvm
lazy val coreJs = core.js

lazy val backend = (project in file("backend"))
  .enablePlugins(WebScalaJSBundlerPlugin, DockerPlugin)
  .settings(commonSettings)
  .settings(
    test in assembly := {},
    assemblyMergeStrategy in assembly := {
      case PathList(ps @ _*) if ps.last endsWith "BuildInfo$.class" => MergeStrategy.first
      case x => {
        val oldStrategy = (assemblyMergeStrategy in assembly).value
        oldStrategy(x)
      }
    },
    mainClass in assembly := Some(Backend.mainClass),
    
    scalaJSProjects := Seq(frontend),
    pipelineStages in Assets := Seq(scalaJSPipeline),
    compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,

    npmAssets ++= NpmAssets.ofProject(frontend) { nodeModules =>
      (
        nodeModules / Ace.keybindingPath +++ 
        nodeModules / Ace.themePath +++
        nodeModules / Ace.modePath
      ).allPaths
    }.value,
    
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-blaze-server" % http4sV,
      "org.http4s" %% "http4s-circe" % http4sV,
      "org.http4s" %% "http4s-dsl" % http4sV,
      "com.github.pureconfig" %% "pureconfig" % pureConfigV,
      "com.github.pureconfig" %% "pureconfig-cats-effect" % pureConfigV,
      "org.fusesource.jansi" % "jansi" % "1.18",
      "ch.qos.logback" % "logback-classic" % "1.2.3"
    ),
    
    WebKeys.packagePrefix in Assets := Backend.assetPath,
    managedClasspath in Runtime += (packageBin in Assets).value,
    
    // Run fastOptJS on reStart
    reStart := (reStart dependsOn (fastOptJS in (frontend, Compile))).evaluated,
    // Run reStart when frontend changes have been made
    watchSources ++= (watchSources in frontend).value,
    // Main class
    mainClass in reStart := Some(Backend.developMainClass),

    buildOptions in docker := BuildOptions(
      cache = false,
      removeIntermediateContainers = BuildOptions.Remove.Always,
      pullBaseImage = BuildOptions.Pull.Always
    ),
    
    dockerfile in docker := {
      val app: File = assembly.value
      val appTarget = s"${Application.name}.jar"
      val port = sys.env.getOrElse("PORT","8090").toInt

      new Dockerfile {
        from("openjdk:8-jre-alpine")
        expose(port)
        add(app, appTarget)
        cmd("java", "-jar", appTarget)
      }
    },

    imageNames in docker := Seq(
      ImageName("registry.heroku.com/concussion-io/web:latest")
    )
  )
  .dependsOn(coreJvm)

val webpackDir = Def.setting {
  (baseDirectory in ThisProject).value / "webpack"
}

val webpackDevConf = Def.setting {
  Some(webpackDir.value / "dev.webpack.config.js")
}

val webpackProdConf = Def.setting {
  Some(webpackDir.value / "prod.webpack.config.js")
}

lazy val frontend = (project in file("frontend"))
  .enablePlugins(ScalaJSPlugin, ScalaJSWeb, ScalaJSBundlerPlugin)
  .settings(commonSettings)
  .settings(
    version in webpack := "4.30.0",
    version in startWebpackDevServer := "3.3.1",
    webpackDevServerExtraArgs := Seq("--inline"),
    webpackResources := webpackDir.value * "*.js",
    webpackConfigFile in fastOptJS := webpackDevConf.value,
    webpackConfigFile in fullOptJS := webpackProdConf.value,
    webpackBundlingMode in fastOptJS := BundlingMode.LibraryOnly(),
    webpackBundlingMode in fullOptJS := BundlingMode.Application,
    includeFilter in webpackMonitoredFiles := "*",
    emitSourceMaps := false,
    scalaJSUseMainModuleInitializer := true,
    useYarn := true,    
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalaJsDomV,
      "com.github.japgolly.scalajs-react" %%% "core" % scalaJsReactV,
      "com.github.japgolly.scalajs-react" %%% "extra" % scalaJsReactV,
      "com.github.japgolly.scalacss" %%% "ext-react" % scalaCssV,
      "io.github.cquiroz.react" %%% "react-semantic-ui" % scalaJsReactSemanticUIV
    ),
    npmDependencies in Compile ++= Seq(
      "react" -> reactV,
      "react-dom" -> reactV,
      "react-ace" -> reactAceV,
      "react-draggable" -> reactDraggableV,
      "semantic-ui-react" -> semanticUIReactV
    ),
    npmDevDependencies in Compile ++= Seq(
      "webpack-merge" -> "4.1.0",
      "html-webpack-plugin" -> "3.1.0",
      "imports-loader" -> "0.8.0",
      "expose-loader" -> "0.7.5",
      "semantic-ui-css" -> semanticUICssV
    )
  )
  .dependsOn(coreJs)

onLoad in Global := { state =>
  // Check for library updates whenever the project is [re]loaded
  if (sys.props.contains(skipDependencyUpdates)) state
  else "dependencyUpdates" :: state
}