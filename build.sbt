lazy val commonSettings = List(
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  addCompilerPlugin(Libraries.paradise),
  organization := "com.alexknvl",
  version := "0.3.1",
  scalaVersion := "2.12.1",
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
    Resolver.mavenLocal,
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.bintrayRepo("alexknvl", "maven"))
)

lazy val core = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(
    name := "sio-core",
    libraryDependencies
      ++= Libraries.cats
      ++ Libraries.simulacrum
      ++ Libraries.leibniz
      ++ Libraries.sourcecode)

lazy val concurrent = (project in file("concurrent"))
  .settings(commonSettings: _*)
  .settings(
    name := "sio-concurrent",
    libraryDependencies
      ++= Libraries.cats
      ++ Libraries.simulacrum)
  .dependsOn(core)

lazy val iteratee = (project in file("iteratee"))
  .settings(commonSettings: _*)
  .settings(
    name := "sio-iteratee",
    libraryDependencies
      ++= Libraries.iteratee)
  .dependsOn(core)

lazy val iterateeFiles = (project in file("iteratee-files"))
  .settings(commonSettings: _*)
  .settings(
    name := "sio-iteratee-files",
    libraryDependencies
      ++= Libraries.iteratee)
  .dependsOn(core, iteratee)

lazy val iterateeGZip = (project in file("iteratee-gzip"))
  .settings(commonSettings: _*)
  .settings(
    name := "sio-iteratee-gzip",
    libraryDependencies
      ++= Libraries.iteratee)
  .dependsOn(core, iteratee, iterateeFiles)

lazy val regions = (project in file("regions"))
  .settings(commonSettings: _*)
  .settings(
    name := "sio-regions",
    libraryDependencies
      ++= Libraries.leibniz)
  .dependsOn(core)

lazy val eff = (project in file("eff"))
  .settings(commonSettings: _*)
  .settings(
    name := "sio-eff",
    libraryDependencies
      ++= Libraries.shapeless)
  .dependsOn(core)

lazy val teletype = (project in file("teletype"))
  .settings(commonSettings: _*)
  .settings(name := "sio-teletype")
  .dependsOn(core)

lazy val macros = (project in file("macros"))
  .settings(commonSettings: _*)
  .settings(
    name := "sio-macros",
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _),
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies += "org.typelevel" %% "macro-compat" % "1.1.1")
  .dependsOn(core)

lazy val example = (project in file("example"))
  .settings(commonSettings: _*)
  .settings(name := "sio-example")
  .dependsOn(core, eff, teletype, regions)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(name := "sio")
  .aggregate(
    core, concurrent,
    eff, regions,
    teletype,
    iteratee, iterateeFiles, iterateeGZip,
    example)
