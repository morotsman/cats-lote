ThisBuild / name := "cats-lote"
ThisBuild / scalaVersion := "2.13.15"
ThisBuild / version := "0.0.1-SNAPSHOT"

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision


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


lazy val lote = (project in file("lote"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      // cats
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.typelevel" %% "cats-effect" % "3.5.7",
      "org.jline" % "jline" % "3.27.1",
      "dev.optics" %% "monocle-core"  % "3.3.0",
      "dev.optics" %% "monocle-macro" % "3.3.0",
      // test
      "org.typelevel" %% "munit-cats-effect" % "2.0.0" % Test,
      "org.scalameta" %% "munit" % "1.0.3" % Test,
    ),
    testFrameworks += new TestFramework("munit.Framework"),
    scalacOptions ++= Seq(
      "-Ymacro-annotations"
    )
  )

lazy val examples = (project in file("examples"))
  .dependsOn(lote % "test->test;compile->compile")
  .settings(commonSettings)
  .settings(
    scalacOptions ++= Seq(
      "-Ymacro-annotations"
    )
  )