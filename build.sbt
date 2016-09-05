name := "ComposableModels"

organization := "com.github.jonnylaw"

version := "0.1"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  Resolver.sonatypeRepo("public")
)

libraryDependencies  ++= Seq(
  "com.typesafe.akka" %% "akka-stream" % "2.4.6",
  "org.scalanlp" %% "breeze" % "0.10",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "com.github.fommil.netlib" % "all" % "1.1.2",
  "org.typelevel" %% "cats" % "0.6.1",
  "com.github.scopt" %% "scopt" % "3.5.0"
)

sonatypeProfileName := "com.github.jonnylaw"

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
