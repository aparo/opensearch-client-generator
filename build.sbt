organization := "io.megl"

name := "codeGenerator"

Licensing.settings

inThisBuild(
  List(
    scalaVersion := "2.12.12",
    organization := "io.megl",
    homepage := Some(url("https://zio.github.io/zio-json/")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "jdegoes",
        "John De Goes",
        "john@degoes.net",
        url("http://degoes.net")
      )
    ),
    parallelExecution := true,
    scalafmtOnCompile := false,
    Compile / doc / sources := Seq.empty,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    semanticdbIncludeInJar := false,
    scalafixScalaBinaryVersion := "2.12",
    scalafixDependencies ++= Seq(
      "com.github.liancheng" %% "organize-imports" % "0.5.0",
      "com.github.vovapolu"  %% "scaluzzi"         % "0.1.17"
    ),
    scmInfo := Some(
      ScmInfo(url("https://github.com/zio/zio-json/"), "scm:git:git@github.com:zio/zio-json.git")
    )
  )
)

version := "1.0.0"

val zioVersion = "1.0.9"

lazy val root = project
  .in(file("."))
  .settings(
    publish / skip := true
  )
  .aggregate(
//    ts,
    generator
  )

//lazy val ts = (project in file("ts"))
//  .settings(BuildHelper.commonSettings)
//  .settings(
//      libraryDependencies ++= Seq(Deps.ammoniteOps, Deps.osLib, Deps.sourcecode, Deps.parserCombinators, Deps.fansi) ++ Deps.circe
//    )

lazy val generator = (project in file("generator"))
  .enablePlugins(SbtTwirl)
  .settings(BuildHelper.commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalablytyped.converter" %% "ts"                           % "1.0.0-beta33",
      "com.google.guava"             % "guava"                        % "23.0",
      "com.typesafe"                 % "config"                       % "1.4.1",
      "ch.qos.logback"               % "logback-classic"              % "1.2.3",
      "com.github.pathikrit"        %% "better-files"                 % "3.9.1",
      "com.typesafe.scala-logging"  %% "scala-logging"                % "3.9.4",
      "io.circe"                    %% "circe-derivation-annotations" % "0.13.0-M5",
      "io.circe"                    %% "circe-parser"                 % "0.14.1",
      "io.circe"                    %% "circe-yaml"                   % "0.14.0",
      "com.beachape"                %% "enumeratum-circe"             % "1.7.0",
      "io.github.java-diff-utils"    % "java-diff-utils"              % "4.10",
      "org.scalameta"               %% "scalafmt-core"                % "2.7.5",
      "dev.zio"                     %% "zio"                          % zioVersion,
      "dev.zio"                     %% "zio-test"                     % zioVersion % "test",
      "dev.zio"                     %% "zio-test-sbt"                 % zioVersion % "test",
      "dev.zio"                     %% "zio-test-magnolia"            % zioVersion % "test" // optional
    )
  ) //.dependsOn(ts)

addCommandAlias("fix", "scalafixAll")
addCommandAlias("fixCheck", "scalafixAll --check")
addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
addCommandAlias("fmtCheck", "all scalafmtSbtCheck scalafmtCheckAll")
addCommandAlias("prepare", "headerCreateAll; fix; fmt")
