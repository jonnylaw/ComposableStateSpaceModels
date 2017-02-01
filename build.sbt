name := "ComposableModels"

organization := "com.github.jonnylaw"

version := "0.1"

scalaVersion := "2.12"

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  Resolver.sonatypeRepo("public")
)

libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "0.12",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "com.github.fommil.netlib" % "all" % "1.1.2",
  "org.typelevel" %% "cats" % "0.9.0",
  "co.fs2" %% "fs2-core" % "0.9.2",
  "co.fs2" %% "fs2-io" % "0.9.2"
)

// sonatypeProfileName := "com.github.jonnylaw"

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("Snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("Releases" at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

apiURL := Some(url("http://jonnylaw.github.io/ComposableStateSpaceModels/"))

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
