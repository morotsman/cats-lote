name := "cats-lote"

version := "0.1"

scalaVersion := "2.13.8"

libraryDependencies ++= Seq(
  // cats
  "org.typelevel" %% "cats-core" % "2.7.0",
  "org.typelevel" %% "cats-effect" % "3.3.11",
  "io.chrisdavenport" %% "circuit" % "0.5.0",
  "org.jline" % "jline" % "3.21.0",
  "dev.optics" %% "monocle-core"  % "3.1.0",
  "dev.optics" %% "monocle-macro" % "3.1.0",
)

idePackagePrefix := Some("com.github.morotsman")

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

scalacOptions ++= Seq(
  "-Ymacro-annotations"
)