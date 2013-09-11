resolvers += Classpaths.typesafeResolver

organization := "net.devkat"

name := "lift-jcr-record"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "net.liftweb" %% "lift-record" % "2.5",
  "javax.jcr" % "jcr" % "2.0",
  "org.jcrom" % "jcrom" % "2.1.0",
  "org.apache.jackrabbit" % "jackrabbit-core" % "2.7.0" % "test",
  "org.specs2" %% "specs2" % "2.2" % "test",
  "org.slf4j" % "slf4j-simple" % "1.7.5"
)

testOptions in Test += Tests.Setup(() => {
    System.setProperty("org.apache.jackrabbit.repository.conf", "classpath:repository.xml")
    System.setProperty("org.apache.jackrabbit.repository.home", "target/repository")
  }
)