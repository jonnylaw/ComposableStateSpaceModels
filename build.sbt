lazy val Benchmark = config("bench") extend Test

/**  This allows running ScalaMeter benchmarks in separate sbt configuration.
  *  It means, that when you want run your benchmarks you should type `bench:test` in sbt console.
  */
lazy val basic = Project(
  "basic-with-separate-config",
  file("."),
  settings = Defaults.coreDefaultSettings ++ Seq(
    name := "ComposableModels",
    organization := "com.github.jonnylaw",
    version := "0.6.0",
    scalaVersion := "2.11.8",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xlint"),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("Snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("Releases" at nexus + "service/local/staging/deploy/maven2")
    },
    apiURL := Some(url("http://jonnylaw.github.io/ComposableStateSpaceModels/")),
    libraryDependencies ++= Seq(
      "org.scalanlp" %% "breeze" % "0.12",
      "com.github.fommil.netlib" % "all" % "1.1.2",
      "org.typelevel" %% "cats" % "0.9.0",
      "com.typesafe.akka" %% "akka-stream" % "2.4.17",
      "com.github.mpilquist" %% "simulacrum" % "0.10.0",
      //"io.spray" %%  "spray-json" % "1.3.3",
      "com.typesafe.akka" %% "akka-http" % "10.0.3",
      "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.5",
      "com.github.nscala-time" %% "nscala-time" % "2.16.0",
      "org.scalatest" %% "scalatest" % "2.2.4" % "test",
      "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
      "com.storm-enroute" %% "scalameter" % "0.7" % "bench"
    ),
    resolvers ++= Seq(
      "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
      "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
      Resolver.sonatypeRepo("public")
    ),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    parallelExecution in Benchmark := false,
    logBuffered := false,
//    sonatypeProfileName := "com.github.jonnylaw",
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    pomExtra := (
      <url>https://github.com/jonnylaw/ComposableStateSpaceModels</url>
          <licenses>
        <license>
        <name>BSD-style</name>
        <url>http://www.opensource.org/licenses/bsd-license.php</url>
          <distribution>repo</distribution>
        </license>
        </licenses>
        <scm>
        <url>https://github.com/jonnylaw/ComposableStateSpaceModels</url>
          <connection>scm:git:git://github.com/jonnylaw/ComposableStateSpaceModels.git</connection>
          </scm>
        <developers>
        <developer>
        <id>jonnylaw</id>
        <name>Jonny Law</name>
        <url>http://www.bigdata-cdt.ac.uk/people/students/jonathanlaw.html</url>
          </developer>
        </developers>)
  )
) configs(
  Benchmark
) settings(
  inConfig(Benchmark)(Defaults.testSettings): _*
)
