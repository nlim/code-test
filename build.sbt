name := "code-test"

version := "0.0.1"

scalaVersion := "2.11.2"

parallelExecution in Test := false

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
)

autoCompilerPlugins := true

scalacOptions += "-feature"
