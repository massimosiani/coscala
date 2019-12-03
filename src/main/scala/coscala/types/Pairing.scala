package coscala.types

import cats.{Comonad, Monad}

trait Pairing {

  def pair[A, B, C, M[_] : Monad, W[_] : Comonad](f: A => B => C): M[A] => W[B] => C
}
