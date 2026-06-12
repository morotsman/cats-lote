ThisBuild / name := "cats-lote"
ThisBuild / scalaVersion := "2.13.12"
ThisBuild / version := "0.0.1-SNAPSHOT"


val commonSettings =
  Seq(
    addCompilerPlugin(
      "org.typelevel" %% "kind-projector" % "0.13.3" cross CrossVersion.full
    ),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
  )


lazy val lote = (project in file("lote"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      // cats
      "org.typelevel" %% "cats-core" % "2.7.0",
      "org.typelevel" %% "cats-effect" % "3.3.11",
      "org.jline" % "jline" % "3.21.0",
      "dev.optics" %% "monocle-core"  % "3.1.0",
      "dev.optics" %% "monocle-macro" % "3.1.0",
      // test
      "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test,
      "org.scalameta" %% "munit" % "0.7.29" % Test,
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