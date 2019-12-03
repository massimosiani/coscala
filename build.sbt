import sbt._

lazy val core = (project in file("."))
  .configs(Configs.all: _*)
  .settings(Settings.core: _*)
