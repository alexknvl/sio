val scalacheck = "org.scalacheck"       %% "scalacheck" % "1.13.4" % "test"
val discipline = "org.typelevel"        %% "discipline" % "0.7.3"  % "test"
val scalatest  = "org.scalatest"        %% "scalatest"  % "3.0.1"  % "test"
val cats       = "org.typelevel"        %% "cats"       % "0.9.0"
val simulacrum = "com.github.mpilquist" %% "simulacrum" % "0.10.0"
val shapeless  = "com.chuusai"          %% "shapeless"  % "2.3.2"
val leibniz    = "com.alexknvl"         %% "leibniz"    % "0.2.1"
val iterateeLibraries = ((version: String) => List(
  "io.iteratee"  %%  "iteratee-core" % version,
  "io.iteratee"  %%  "iteratee-files" % version))
  .apply("0.8.0")

lazy val commonSettings = List(
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  organization := "com.alexknvl",
  version := "0.2.2",
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
    Resolver.mavenLocal,
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"))
)

lazy val core = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(
    name := "sio-core",
    libraryDependencies += cats,
    libraryDependencies += simulacrum,
    libraryDependencies += leibniz)

lazy val concurrent = (project in file("concurrent"))
  .settings(commonSettings: _*)
  .settings(
    name := "sio-concurrent",
    libraryDependencies += cats,
    libraryDependencies += simulacrum)
  .dependsOn(core)

lazy val iteratee = (project in file("iteratee"))
  .settings(commonSettings: _*)
  .settings(
    name := "sio-iteratee",
    libraryDependencies ++= iterateeLibraries)
  .dependsOn(core)

lazy val iterateeFiles = (project in file("iteratee-files"))
  .settings(commonSettings: _*)
  .settings(
    name := "sio-iteratee-files",
    libraryDependencies ++= iterateeLibraries)
  .dependsOn(core, iteratee)

lazy val iterateeGZip = (project in file("iteratee-gzip"))
  .settings(commonSettings: _*)
  .settings(
    name := "sio-iteratee-gzip",
    libraryDependencies ++= iterateeLibraries)
  .dependsOn(core, iteratee, iterateeFiles)

lazy val regions = (project in file("regions"))
  .settings(commonSettings: _*)
  .settings(
    name := "sio-regions",
    libraryDependencies += leibniz)
  .dependsOn(core)

lazy val eff = (project in file("eff"))
  .settings(commonSettings: _*)
  .settings(
    name := "sio-eff",
    libraryDependencies += shapeless)
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
