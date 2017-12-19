lazy val root = (project in file(".")).
  settings(
    name := "Partie1Cluster",
    version := "0.1",
    scalaVersion := "2.11.8",
    mainClass in Compile := Some("myPackage.MyMainObject")
  ).enablePlugins(AssemblyPlugin)


libraryDependencies ++= Seq(
  "org.apache.spark" % "spark-core_2.11" % "2.2.0" % "provided" exclude ("org.apache.hadoop","hadoop-yarn-server-web-proxy") ,
  "org.apache.spark" % "spark-graphx_2.11" % "2.2.0" % "provided"exclude ("org.apache.hadoop","hadoop-yarn-server-web-proxy") ,
  "org.jsoup" % "jsoup" % "1.11.1"//,
  //"net.liftweb" % "lift-json" % "2.6.3"
)

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}
