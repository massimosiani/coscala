package coscala.types

import cats.implicits._
import cats.data.{State, Store}
import cats.{Comonad, Monad}

abstract class Pairing[M[_]: Monad, W[_]: Comonad] {
  def pair[A, B, C](f: A => B => C): M[A] => W[B] => C
}

object Pairing {

  type StateType = List[Strip[Boolean]]
  type StateBool[A] = State[StateType, A]
  type StoreBool[A] = Store[StateType, A]

  def select[A, B, M[_]: Monad, W[_]: Comonad](mb: M[B])(wwa: W[W[A]])(implicit p: Pairing[M, W]): W[A] =
    p.pair[B, W[A], W[A]](_ => identity)(mb)(wwa)

  implicit object StateStorePairing extends Pairing[StateBool, StoreBool] {
    override def pair[A, B, C](f: A => B => C): StateBool[A] => StoreBool[B] => C =
      state =>
        store => {
          val (next, value) = state.run(store.index).value
          f(value)(store.fa(next))
        }
  }
}
