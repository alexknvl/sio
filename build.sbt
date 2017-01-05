val testLibraries = List(
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.typelevel" %% "discipline" % "0.7.3" % "test",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test")

val catsLibraries = List(
  "org.typelevel" %% "algebra" % "0.6.0",
  "org.typelevel" %% "cats" % "0.8.1")

val simulacrumLibrary = List(
  "com.github.mpilquist" %% "simulacrum" % "0.10.0")

val shapelessLibrary = List(
  "com.chuusai" %% "shapeless" % "2.3.2")

val iterateeLibraries = ((version: String) => List(
  "io.iteratee"  %%  "iteratee-core" % version,
  "io.iteratee"  %%  "iteratee-files" % version))
  .apply("0.8.0")

lazy val commonSettings = List(
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  organization := "com.alexknvl",
  version := "0.2",
  scalaVersion := "2.12.0",
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  scalacOptions ++= List(
    "-deprecation", "-unchecked", "-feature",
    "-encoding", "UTF-8",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-Ypartial-unification",
    "-Yno-adapted-args", "-Ywarn-dead-code",
    "-Ywarn-numeric-widen", "-Xfuture"),
  resolvers ++= List(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")),
  libraryDependencies ++= testLibraries
  // wartremoverWarnings ++= Warts.all
)

lazy val core = (project in file("core")).
  settings(name := "sio-core").
  settings(commonSettings: _*).
  settings(libraryDependencies ++=
    catsLibraries ++ simulacrumLibrary)

lazy val concurrent = (project in file("concurrent")).
  settings(name := "sio-concurrent").
  settings(commonSettings: _*).
  settings(libraryDependencies ++=
    catsLibraries ++ simulacrumLibrary).
  dependsOn(core)

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
    core, concurrent,
    eff, regions,
    teletype,
    iteratee, iterateeFiles, iterateeGZip,
    example)
