name := "scala-sbt-cross-compile"

organization := "sample"

version := "1.0"

crossScalaVersions := Seq("2.11.5", "2.10.4")

scalaVersion := "2.11.5"

// add scala-xml dependency when needed (for Scala 2.11 and newer) in a robust way
// this mechanism supports cross-version publishing
libraryDependencies := {
    CrossVersion.partialVersion(scalaVersion.value) match {
        // if scala 2.11+ is used, add dependency on scala-xml module
        case Some((2, scalaMajor)) if scalaMajor >= 11 =>
            libraryDependencies.value ++ Seq(
                "org.scala-lang.modules" %% "scala-xml" % "1.0.3",
                "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
                "org.scala-lang.modules" %% "scala-swing" % "1.0.1",
                "commons-io" % "commons-io" % "2.4",
                "com.github.tototoshi" %% "scala-csv" % "1.2.2",
                "com.lambdaworks" %% "jacks" % "2.3.3")
        case _ =>
            // or just libraryDependencies.value if you don't depend on scala-swing
            libraryDependencies.value :+ "org.scala-lang" % "scala-swing" % scalaVersion.value
    }
}