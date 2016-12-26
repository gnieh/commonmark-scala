import scalariform.formatter.preferences._

organization := "org.gnieh"

name := "commonmark-scala"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.1"

scalacOptions ++= Seq("-deprecation", "-feature")

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.unbescape" % "unbescape" % "1.1.4.RELEASE"

scalariformSettings

ScalariformKeys.preferences := {
  ScalariformKeys.preferences.value
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentClassDeclaration, true)
    .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
}
