/**
  * sbt Microsite Configuration
  */
enablePlugins(MicrositesPlugin)

lazy val micrositeSettings = Seq(
  micrositeName := "Composable State Space Models",
  micrositeDescription := "Bayesian Inference for Composable State Space Models",
  micrositeBaseUrl := "/ComposableStateSpaceModels",
  micrositeDocumentationUrl := "/ComposableStateSpaceModels/docs",
  micrositeGithubOwner := "jonnylaw",
  micrositeGithubRepo := "ComposableStateSpaceModels",
  micrositeImgDirectory := (resourceDirectory in Compile).value / "site" / "figures",
  micrositeCssDirectory := (resourceDirectory in Compile).value / "site" / "styles",
  micrositeHighlightTheme := "solarized-dark",
  micrositeCDNDirectives := microsites.CdnDirectives(
    jsList = List(
      "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-MML-AM_CHTML"
    ))
)

lazy val basic = Project(
  "basic-with-separate-config",
  file("."),
  settings = Defaults.coreDefaultSettings ++ micrositeSettings ++ Seq(
    name := "ComposableModels",
    organization := "com.github.jonnylaw",
    scalaVersion := "2.12.1",
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
      "org.scalanlp" %% "breeze" % "0.13",
      "com.github.fommil.netlib" % "all" % "1.1.2",
      "org.typelevel" %% "cats" % "0.9.0",
      "org.typelevel" %% "cats-laws" % "0.9.0",
      "com.typesafe.akka" %% "akka-stream" % "2.4.17",
      "com.github.mpilquist" %% "simulacrum" % "0.10.0",
      "io.spray" %%  "spray-json" % "1.3.3",
      "com.github.nscala-time" %% "nscala-time" % "2.16.0",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test",
      "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
    ),
    resolvers ++= Seq(
      "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
      "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
      Resolver.sonatypeRepo("public")
    ),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    logBuffered := false,
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
)
