enablePlugins(OssLibPlugin)

organization := "com.thenewmotion"

name := "scala-kendoui-slick"

scalaVersion := tnm.ScalaVersion.prev
crossScalaVersions := Seq(tnm.ScalaVersion.prev, tnm.ScalaVersion.aged)

libraryDependencies ++= {
  val liftVersion = "3.4.0"

  Seq(
    "com.typesafe.slick" %% "slick" % "2.1.0",
    "org.slf4j" % "slf4j-api" % "1.7.29",

    "net.liftweb" %% "lift-webkit" % liftVersion,
    "net.liftweb" %% "lift-json-ext" % liftVersion,

    "com.h2database" % "h2" % "1.4.200" % "test",
    "org.specs2" %% "specs2-core" % "4.8.1" % "test"
  )
}
