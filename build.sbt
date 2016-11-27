val monocleLibraries = ((version: String) => List(
  "com.github.julien-truffaut"  %%  "monocle-core" % version,
  "com.github.julien-truffaut"  %%  "monocle-generic" % version,
  "com.github.julien-truffaut"  %%  "monocle-macro" % version,
  "com.github.julien-truffaut"  %%  "monocle-state" % version,
  "com.github.julien-truffaut"  %%  "monocle-refined" % version,
  "com.github.julien-truffaut"  %%  "monocle-law" % version % "test"))
  .apply("1.3.0")

val testLibraries = List(
  "org.scalacheck" %% "scalacheck" % "1.13.2" % "test",
  "org.typelevel" %% "discipline" % "0.7" % "test",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test")

val effLibrary = List(
  "org.atnos" %% "eff-cats" % "2.0.0-RC17")

val catsLibraries = List(
  "org.typelevel" %% "algebra" % "0.5.1",
  "org.typelevel" %% "cats" % "0.7.2",
  "org.typelevel" %% "dogs-core" % "0.3.1")

val simulacrumLibrary = List(
  "com.github.mpilquist" %% "simulacrum" % "0.8.0")

val refinedLibrary = List(
  "eu.timepit" %% "refined" % "0.5.0")

val shapelessLibrary = List(
  "com.chuusai" %% "shapeless" % "2.3.2")

val iterateeLibraries = ((version: String) => List(
  "io.iteratee"  %%  "iteratee-core" % version,
  "io.iteratee"  %%  "iteratee-files" % version))
  .apply("0.6.1")

lazy val commonSettings = List(
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.8.0"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  organization := "com.alexknvl",
  version := "0.1-SNAPSHOT",
  scalaOrganization := "org.typelevel",
  scalaVersion := "2.11.8",
  scalacOptions ++= List(
    "-deprecation", "-unchecked", "-feature",
    "-encoding", "UTF-8",
    "-language:existentials", "-language:higherKinds",
    "-Yno-adapted-args", "-Ywarn-dead-code",
    "-Ywarn-numeric-widen", "-Xfuture",
    "-Ypartial-unification", "-Yliteral-types"),
  resolvers ++= List(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")),
  libraryDependencies ++= testLibraries,
  wartremoverWarnings ++= Warts.all)

lazy val core = (project in file("core")).
  settings(name := "sio-core").
  settings(commonSettings: _*).
  settings(libraryDependencies ++=
    catsLibraries ++
    simulacrumLibrary)

lazy val iteratee = (project in file("iteratee")).
  settings(name := "sio-iteratee").
  settings(commonSettings: _*).
  settings(libraryDependencies ++= iterateeLibraries).
  dependsOn(core)

lazy val iterateeFiles = (project in file("iteratee-files")).
  settings(name := "sio-iteratee-files").
  settings(commonSettings: _*).
  settings(libraryDependencies ++= iterateeLibraries).
  dependsOn(core, iteratee)

lazy val iterateeGZip = (project in file("iteratee-gzip")).
  settings(name := "sio-iteratee-gzip").
  settings(commonSettings: _*).
  settings(libraryDependencies ++= iterateeLibraries).
  dependsOn(core, iteratee, iterateeFiles)

lazy val regions = (project in file("regions")).
  settings(name := "sio-regions").
  settings(commonSettings: _*).
  dependsOn(core)

lazy val eff = (project in file("eff")).
  settings(name := "sio-eff").
  settings(commonSettings: _*).
  settings(libraryDependencies ++= shapelessLibrary).
  dependsOn(core)

lazy val teletype = (project in file("teletype")).
  settings(name := "sio-teletype").
  settings(commonSettings: _*).
  dependsOn(core)

lazy val macros = (project in file("macros")).
  settings(name := "sio-macros").
  settings(commonSettings: _*).
  settings(
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _),
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies += "org.typelevel" %% "macro-compat" % "1.1.1").
  dependsOn(core)

lazy val example = (project in file("example")).
  settings(name := "sio-example").
  settings(commonSettings: _*).
  dependsOn(core, eff, teletype, regions)

lazy val root = (project in file(".")).
  settings(name := "sio").
  settings(commonSettings: _*).
  aggregate(
    core, eff, regions,
    teletype,
    iteratee, iterateeFiles, iterateeGZip,
    example)
