import sbt.Keys._
import sbt._

object Dependencies {

  object Version {
    val cats = "2.0.0"
    val scalaTest = "3.1.0"
  }

  val coscala: Seq[Setting[_]] = deps(
    "org.typelevel" %% "cats-core" % Version.cats,
    "org.typelevel" %% "cats-effect" % Version.cats,
    "io.estatico" %% "newtype" % "0.4.3",
    "org.scalatest" %% "scalatest" % Version.scalaTest,
    "org.typelevel" %% "cats-laws" % Version.cats % Test,
    "org.typelevel" %% "discipline-scalatest" % "1.0.0-RC1",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3" % Test
  )

  private def deps(modules: ModuleID*): Seq[Setting[_]] = Seq(libraryDependencies ++= modules)
}
