name := "scalaz-book"

version := "0.1"

scalaVersion := "2.12.6"

scalacOptions in ThisBuild ++= Seq(
  "-language:_",
  "-Ypartial-unification"
//  "-Xfatal-warnings"
)
libraryDependencies ++= Seq(
  "com.github.mpilquist" %% "simulacrum" % "0.12.0",
  "org.scalaz" %% "scalaz-core" % "7.2.22"
)

libraryDependencies ++= Seq(
  "eu.timepit" %% "refined"            % "0.9.2",
  "eu.timepit" %% "refined-cats"       % "0.9.2", // optional
  "eu.timepit" %% "refined-eval"       % "0.9.2", // optional, JVM-only
  "eu.timepit" %% "refined-jsonpath"   % "0.9.2", // optional, JVM-only
  "eu.timepit" %% "refined-pureconfig" % "0.9.2", // optional, JVM-only
  "eu.timepit" %% "refined-scalacheck" % "0.9.2", // optional
  "eu.timepit" %% "refined-scalaz"     % "0.9.2", // optional
  "eu.timepit" %% "refined-scodec"     % "0.9.2", // optional
  "eu.timepit" %% "refined-scopt"      % "0.9.2", // optional
  "eu.timepit" %% "refined-shapeless"  % "0.9.2"  // optional
)

libraryDependencies += "org.typelevel" %% "cats-effect" % "1.0.0-RC2"

// https://mvnrepository.com/artifact/com.google.guava/guava
libraryDependencies += "com.google.guava" % "guava" % "12.0"

libraryDependencies += "xyz.driver" %% "spray-json-derivation" % "0.4.1"


libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

/*
import scalaz._, Scalaz._
import simulacrum._

type constructor (eg list_

 */