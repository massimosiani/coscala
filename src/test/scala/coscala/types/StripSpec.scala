package coscala.types

import cats.implicits._
import cats.kernel.Eq
import cats.laws.discipline.ComonadTests
import coscala.types.arbitraries._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.typelevel.discipline.scalatest.Discipline

object arbitraries {
  implicit def arbStrip: Arbitrary[Strip[Int]] = Arbitrary(
    for {
      list <- Arbitrary.arbitrary[IndexedSeq[Int]] suchThat { _.nonEmpty }
      index <- Gen.choose(0, list.size - 1)
    } yield Strip(list, index)
  )

  implicit def eqStrip[A: Eq]: Eq[Strip[A]] = Eq.fromUniversalEquals

  implicit def cogenStrip[A]: Cogen[Strip[A]] = Cogen(_ => 7)
}

class StripSpec extends AnyFunSuite with Discipline {

  checkAll("Strip.ComonadLaws", ComonadTests[Strip].comonad[Int, Int, String])
}
