enablePlugins(OssLibPlugin)

organization := "com.thenewmotion"

name := "scala-kendoui-slick"

crossScalaVersions := Seq(tnm.ScalaVersion.prev)

libraryDependencies ++= {
  val liftVersion = "2.6.2"

  Seq(
    "com.typesafe.slick" %% "slick" % "2.1.0",
    "org.slf4j" % "slf4j-api" % "1.7.12",

    "net.liftweb" %% "lift-webkit" % liftVersion,
    "net.liftweb" %% "lift-json-ext" % liftVersion,

    "com.h2database" % "h2" % "1.4.186" % "test",
    "org.specs2" %% "specs2-core" % "3.6.1" % "test"
  )
}
