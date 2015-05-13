scalaVersion := "2.11.6"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)

libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.3.0"

libraryDependencies += "org.spire-math" %% "cats-core" % "0.1.0-SNAPSHOT"

libraryDependencies += "org.spire-math" %% "cats-std" % "0.1.0-SNAPSHOT"

libraryDependencies += "org.spire-math" %% "cats-laws" % "0.1.0-SNAPSHOT"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.1"

libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.1.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.1"