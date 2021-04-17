organization := "io.megl"

name := "codeGenerator"

inThisBuild(
  Seq(
    scalaVersion := "2.13.5",
    parallelExecution := true,
    scalafmtOnCompile := false,
    Compile / doc / sources := Seq.empty,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    semanticdbIncludeInJar := false,
    scalafixScalaBinaryVersion := "2.13",
    scalafixDependencies ++= Seq("com.github.liancheng" %% "organize-imports" % "0.5.0")
  )
)

version := "1.0.0"

lazy val root = (project in file(".")).enablePlugins(SbtTwirl)

val zioVersion = "1.0.5"

Licensing.settings

libraryDependencies ++= Seq(
  "com.google.guava"            % "guava"                        % "23.0",
  "com.typesafe"                % "config"                       % "1.4.0",
  "ch.qos.logback"              % "logback-classic"              % "1.2.3",
  "com.github.pathikrit"       %% "better-files"                 % "3.9.1",
  "com.typesafe.scala-logging" %% "scala-logging"                % "3.9.3",
  "io.circe"                   %% "circe-derivation-annotations" % "0.13.0-M5",
  "io.circe"                   %% "circe-parser"                 % "0.13.0",
  "io.circe"                   %% "circe-yaml"                   % "0.12.0",
  "com.beachape"               %% "enumeratum-circe"             % "1.6.1",
  "io.github.java-diff-utils"   % "java-diff-utils"              % "4.9",
  "org.scalameta"              %% "scalafmt-core"                % "2.7.5",
  "dev.zio"                    %% "zio"                          % zioVersion,
  "dev.zio"                    %% "zio-test"                     % zioVersion % "test",
  "dev.zio"                    %% "zio-test-sbt"                 % zioVersion % "test",
  "dev.zio"                    %% "zio-test-magnolia"            % zioVersion % "test" // optional
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

addCompilerPlugin(
  "org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full
)
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

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
  "-Ymacro-annotations",
  "-Xmacro-settings:materialize-derivations"
)

addCommandAlias("fix", "scalafixAll")
addCommandAlias("fixCheck", "scalafixAll --check")
addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
addCommandAlias("fmtCheck", "all scalafmtSbtCheck scalafmtCheckAll")
addCommandAlias("prepare", "headerCreateAll; fix; fmt")
