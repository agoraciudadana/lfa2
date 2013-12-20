name := "lfa"

version := "1.0"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(    
    "com.michaelpollmeier" % "gremlin-scala" % "2.4.1",
    "com.tinkerpop" % "furnace" % "0.1-SNAPSHOT"
)

resolvers ++= Seq(
  //"Local Maven Repository" at Path.userHome.asFile.toURI.toURL + "/.m2/repository",
  "Maven Central" at "http://repo1.maven.org/maven2/",
  "Sonatype releases" at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Aduna Software" at "http://repo.aduna-software.org/maven2/releases/", //for org.openrdf.sesame
  "Restlet Framework" at "http://maven.restlet.org"
)