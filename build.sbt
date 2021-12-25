lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc2021",
    version := "0.1.0",

    scalaVersion := "3.1.0",
    scalacOptions ++= Seq(
      "-Yexplicit-nulls",
      "-language:strict-equality",
      "-deprecation",
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.6.1",
      "org.typelevel" %% "cats-effect" % "3.1.1",
      "org.typelevel" %% "kittens" % "3.0.0-M1",
      "org.tpolecat" %% "atto-core" % "0.9.5",

      ),
    console / initialCommands := """
    import de.qwertyuiop.aoc.inputSource, de.qwertyuiop.aoc.lib.{*, given}, de.qwertyuiop.aoc.`2021`.{*, given}
    import cats.*, cats.data.*, cats.implicits.given
    import scala.util.chaining.given
    """
  )

Runtime / unmanagedSources += baseDirectory.value / "input"
