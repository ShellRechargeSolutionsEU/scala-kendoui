
val liftVersion = "2.5.1"

val commonSettings = Seq(
  organization := "com.thenewmotion",
  scalaVersion := "2.10.4",
  libraryDependencies ++= Seq(
    "com.h2database"                  % "h2"                          % "1.4.186"             % "test",
    "org.specs2"                      %% "specs2-junit"               % "2.4.17"              % "test"
  )
)

lazy val common = project.in(file("common"))
  .enablePlugins(OssLibPlugin)
  .settings(commonSettings: _*)
  .settings(
    name := "scala-kendoui-common",
    libraryDependencies ++= Seq(
      "com.thenewmotion"                %% "time"                       % "2.8",
      "com.typesafe.scala-logging"      %% "scala-logging-slf4j"        % "2.1.2",
      "ua.t3hnar.scalax"                %% "scalax"                     % "1.8",
      "net.liftweb"                     %% "lift-webkit"                % liftVersion,
      "net.liftweb"                     %% "lift-json-ext"              % liftVersion
    )
  )

lazy val slick = project
  .enablePlugins(OssLibPlugin)
  .dependsOn(common)
  .settings(commonSettings: _*)
  .settings(
    name := "scala-kendoui-slick"
  )
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.slick"              %% "slick"                      % "2.1.0"
    )
  )

lazy val squeryl = project
  .enablePlugins(OssLibPlugin)
  .dependsOn(common)
  .settings(commonSettings: _*)
  .settings(
    name := "scala-kendoui-squeryl"
  )
  .settings(
    libraryDependencies ++= Seq(
      "org.squeryl"                     %% "squeryl"                    % "0.9.5-6"
    )
  )

packagedArtifacts in file(".") := Map.empty
