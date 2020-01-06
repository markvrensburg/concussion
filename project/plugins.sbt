// Fast dependency resolver
addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.3")

// Continuously reload applications
addSbtPlugin("io.spray" % "sbt-revolver" % "0.9.1")

// Allows Scala.js Compilation
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.1")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.31")
addSbtPlugin("ch.epfl.scala" % "sbt-web-scalajs-bundler-sjs06" % "0.16.0")

// Extract metadata from sbt and make it available to the code
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.9.0")

// Best practices for production code
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.4")
addSbtPlugin("org.wartremover" % "sbt-wartremover" % "2.4.2")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.0.0")

// Dependency utilities
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.3.4")

// Application packaging
addSbtPlugin("se.marcuslonnberg" % "sbt-docker" % "1.5.0")
