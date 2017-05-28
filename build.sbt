name := "songday.server"
version := "1.0"
scalaVersion  := "2.11.7"
scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")
val sprayV = "1.3.2"
resolvers ++= Seq(
  "spray repo" at "http://repo.spray.io",
  "spray nightlies repo" at "http://nightlies.spray.io",
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "jvnet mimepull" at "http://mimepull.java.net"
)
 
libraryDependencies += "io.spray" %% "spray-can" % sprayV
libraryDependencies += "io.spray" %% "spray-httpx" % sprayV
libraryDependencies += "io.spray" %% "spray-routing" % sprayV
libraryDependencies += "io.spray" %% "spray-json" % "1.3.2"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.2"
libraryDependencies += "com.typesafe.akka" %% "akka-actor"   % "2.3.14"
libraryDependencies += "com.typesafe.akka" %% "akka-slf4j"   % "2.3.14"
libraryDependencies += "com.typesafe.akka" %% "akka-remote"   % "2.3.14"
libraryDependencies += "com.typesafe.akka" %% "akka-agent"   % "2.3.14"
libraryDependencies += "org.jvnet.mimepull" % "mimepull" % "1.9.6"
libraryDependencies += "org.json4s" % "json4s-native_2.11" % "3.3.0"
libraryDependencies += "net.debasishg" %% "redisclient" % "3.0"
libraryDependencies += "com.pauldijou" %% "jwt-core" % "0.4.1"

