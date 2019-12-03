import sbt._

lazy val coscala = (project in file("."))
  .configs(Configs.all: _*)
  .settings(Settings.coscala: _*)
