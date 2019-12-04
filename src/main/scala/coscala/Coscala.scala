package coscala

import cats.implicits._
import cats.data.{State, Store}
import coscala.types.{Pairing, Strip}
import Pairing.StateStorePairing

object Coscala extends App {

  implicit private class CoscalaOps(s: Strip[Boolean]) {
    def w30: Strip[Boolean] = s.coflatMap { s1 =>
      val left = s1.shiftLeft.extract
      val middle = s1.extract
      val right = s1.shiftRight.extract
      (left && !middle && !right) || (!left && middle && right) || (!left && middle && !right) || (!left && !middle && right)
    }

    def render: String = s.as.map(if (_) "X" else " ").mkString
  }

  private def stripStore(start: Strip[Boolean]): Store[List[Strip[Boolean]], String] =
    Store(_.map(_.render).mkString("\n"), List(start))

  private val size = 30
  private val iterations = 90
  private val start: Strip[Boolean] = Strip(
    (IndexedSeq.fill(size)(false) :+ true) ++ IndexedSeq.fill(size)(false),
    0
  )

  private def actions(steps: Int): State[List[Strip[Boolean]], Unit] =
    if (steps == 0) State.pure(())
    else
      for {
        ls <- State.get
        _ <- State.modify((list: List[Strip[Boolean]]) => list ++ List(ls.last.w30))
        result <- actions(steps - 1)
      } yield result

  private val actualStripStore: Store[List[Strip[Boolean]], String] = stripStore(start)

  private val solution = Pairing.select(actions(iterations))(actualStripStore.coflatten)
  println(solution.extract)
}
