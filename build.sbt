name         := "Stadtpark"

version      := "0.1.0-SNAPSHOT"

organization := "de.sciss"

scalaVersion := "2.10.2"

description  := "Algorithms for a sound piece"

homepage     := Some(url("https://github.com/Sciss/" + name.value))

licenses     := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

libraryDependencies ++= Seq(
  // "de.sciss"      %% "strugatzki"          % "2.2.+",
  // "de.sciss"      %% "fscapejobs"          % "1.4.+",
  // "de.sciss"      %% "fileutil"            % "1.0.+",
  // "de.sciss"      %% "lucreconfluent-core" % "2.5.+"
  "de.sciss" %% "mellite" % "0.5.+"
)

// retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

// ---- console ----

initialCommands in console :=
"""import de.sciss.osc
  |import de.sciss.synth._
  |import ugen._
  |import Predef.{any2stringadd => _}
  |import Ops._
  |""".stripMargin

