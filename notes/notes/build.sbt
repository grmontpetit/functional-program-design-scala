name := "week1-notes"

version := "0.0.1"

organization := "sniggel"

scalaVersion  := "2.11.4"

scalacOptions := Seq("-feature", "-unchecked", "-deprecation", "-encoding", "utf8")

fork := true

libraryDependencies ++= {
  Seq(
    "org.scalatest" %% "scalatest" % "3.0.4" % Test
  )
}


