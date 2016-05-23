name := "StreamingIO"

version := "1.0"

scalaVersion := "2.10.6"

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  Resolver.sonatypeRepo("public")
)

val akkaV = "2.0.4"

libraryDependencies  ++= Seq(
  "com.github.nscala-time" %% "nscala-time" % "2.8.0", // joda time
  "com.typesafe.akka" %% "akka-stream-experimental" % akkaV,
  "com.typesafe.akka" % "akka-http-core-experimental_2.10" % akkaV,
  "com.typesafe.akka" %% "akka-http-experimental" % akkaV,
  "io.spray" %%  "spray-json" % "1.3.2",
  "com.typesafe.akka" %% "akka-http-spray-json-experimental" % akkaV,

  "org.scalanlp" %% "breeze" % "0.10", // breeze linear algebra library
  "org.scalanlp" %% "breeze-viz" % "0.11.2",
  // "org.scalaz" %% "scalaz-core" % "7.2.0", // scalaz
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalikejdbc" %% "scalikejdbc"       % "2.3.5",
  "postgresql" % "postgresql" % "9.1-901-1.jdbc4",
  "org.spire-math" %% "spire" % "0.11.0"
  // "org.apache.spark" % "spark-core_2.10" % "1.6.1" % "provided",
  // "org.apache.spark" % "spark-sql_2.10" % "1.6.1" % "provided",
  // "org.apache.hadoop" % "hadoop-client" % "2.6.4" % "provided"
)
