ThisBuild / name := "cats-lote"
ThisBuild / scalaVersion := "2.13.3"
ThisBuild / version := "0.0.1-SNAPSHOT"


val commonSettings =
  Seq(
    addCompilerPlugin(
      "org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full
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
    ),
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