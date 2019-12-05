package coscala

import cats.{Comonad, Monad}
import cats.data.{State, Store}
import coscala.types.{Pairing, Strip}

object Instances {

  def select[A, B, M[_]: Monad, W[_]: Comonad](mb: M[B])(wwa: W[W[A]])(implicit p: Pairing[M, W]): W[A] =
    p.pair[B, W[A], W[A]](_ => identity)(mb)(wwa)

  def storeFrom(start: Strip[Boolean]): Store[List[Strip[Boolean]], String] =
    Store(_.map(_.render).mkString("\n"), List(start))

  def stateAfter(steps: Int): State[List[Strip[Boolean]], Unit] =
    if (steps == 0) State.pure(())
    else
      for {
        ls <- State.get
        _ <- State.modify((list: List[Strip[Boolean]]) => list :+ ls.last.w30)
        result <- stateAfter(steps - 1)
      } yield result

  implicit private class CoscalaOps(s: Strip[Boolean]) {
    def w30: Strip[Boolean] = s.coflatMap { s1 =>
      val left = s1.shiftLeft.extract
      val middle = s1.extract
      val right = s1.shiftRight.extract
      (left && !middle && !right) || (!left && middle && right) || (!left && middle && !right) || (!left && !middle && right)
    }

    def render: String = s.as.map(if (_) "X" else " ").mkString
  }
}
