name := "scalabst"

version := "1.0"

scalaVersion := "2.11.0"

resolvers ++= Seq(
  "Artima Maven Repository" at "http://repo.artima.com/releases"
)

libraryDependencies  ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scalactic" %% "scalactic" % "3.0.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)

// for debugging sbt problems
logLevel := Level.Debug

scalacOptions += "-deprecation"

