import sbt._

class reStructuredTextParserProject(info: ProjectInfo) extends DefaultProject(info) {
  val specsRepo = "Specs Repository" at "http://specs.googlecode.com/svn/maven2/"

  val specs = "org.scala-tools.testing" % "specs" % "1.6.2"
}
