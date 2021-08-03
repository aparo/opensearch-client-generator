import sbt._
import sbt.Keys._

object BuildHelper {
  val commonSettings = Seq(
    scalacOptions ++= Seq(
      "-encoding",
      "UTF-8",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:postfixOps",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:experimental.macros",
      "-Ywarn-unused",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Yrangepos",
      "-Ypatmat-exhaust-depth",
      "off",
//      "-Ymacro-annotations",
      "-Xmacro-settings:materialize-derivations"
    ),
    addCompilerPlugin(("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full)),
    addCompilerPlugin(
      "org.typelevel" %% "kind-projector" % "0.13.0" cross CrossVersion.full
    ),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  ) ++ Licensing.settings
}
