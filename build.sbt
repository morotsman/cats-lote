ThisBuild / scalaVersion := "2.13.15"
ThisBuild / version := "0.0.1-SNAPSHOT"

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

import sbtcrossproject.CrossPlugin.autoImport._

val commonSettings =
  Seq(
    addCompilerPlugin(
      "org.typelevel" %% "kind-projector" % "0.13.3" cross CrossVersion.full
    ),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    scalacOptions ++= Seq(
      "-Wunused:imports",
      "-Wunused:privates",
      "-Wunused:locals",
      "-Wunused:params",
      "-Wunused:patvars"
    )
  )


lazy val lote = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("lote"))
  .settings(commonSettings)
  .settings(
    coverageExcludedFiles := ".*Symbols.*",
    libraryDependencies ++= Seq(
      // cats
      "org.typelevel" %%% "cats-core" % "2.12.0",
      "org.typelevel" %%% "cats-effect" % "3.5.7",
      // test
      "org.typelevel" %%% "munit-cats-effect" % "2.0.0" % Test,
      "org.scalameta" %%% "munit" % "1.0.3" % Test,
      "org.typelevel" %%% "cats-effect-testkit" % "3.5.7" % Test,
    ),
    testFrameworks += new TestFramework("munit.Framework"),
    scalacOptions ++= Seq(
      "-Ymacro-annotations"
    )
  )
  .jvmSettings(
    libraryDependencies += "org.jline" % "jline" % "3.27.1"
  )
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0"
  )

lazy val loteJVM = lote.jvm
lazy val loteJS  = lote.js

lazy val examples = (project in file("examples"))
  .dependsOn(loteJVM % "test->test;compile->compile")
  .settings(commonSettings)
  .settings(
    testFrameworks += new TestFramework("munit.Framework"),
    scalacOptions ++= Seq(
      "-Ymacro-annotations"
    )
  )

lazy val browserExamples = (project in file("browser-examples"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(loteJS % "test->test;compile->compile")
  .settings(commonSettings)
  .settings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass := Some("com.github.morotsman.examples.AdvancedExample"),
    testFrameworks += new TestFramework("munit.Framework"),
    scalacOptions ++= Seq(
      "-Ymacro-annotations"
    )
  )

lazy val root = (project in file("."))
  .aggregate(loteJVM, loteJS, examples, browserExamples)
  .settings(
    name := "cats-lote"
  )
