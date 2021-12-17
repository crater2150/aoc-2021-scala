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

      )
  )

Runtime / unmanagedSources += baseDirectory.value / "input"
