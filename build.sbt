lazy val commonSettings = Seq(
  organization := "com.feynmanliang",
  version := "0.1.0",
  scalaVersion := "2.11.7"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "keywordspotting",
    libraryDependencies := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        // if scala 2.11+ is used, add dependency on scala-xml module
        case Some((2, scalaMajor)) if scalaMajor >= 11 =>
        libraryDependencies.value ++ Seq(
          "org.scala-lang.modules" %% "scala-xml" % "1.0.3")
        case _ =>
        // or just libraryDependencies.value if you don't depend on scala-swing
        libraryDependencies.value
      }
    },
    libraryDependencies += "org.scalactic" %% "scalactic" % "2.2.6",
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0",
    resolvers += Resolver.sonatypeRepo("public")
  )

