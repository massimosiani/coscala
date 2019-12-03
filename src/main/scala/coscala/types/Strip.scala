package coscala.types

import cats.Comonad

case class Strip[A](as: Seq[A], index: Int) {

  def extract: A = as(index)

  def coflatMap[B](f: Strip[A] => B): Strip[B] = Strip(as.indices.map(i => f(Strip(as, i))), index)
}

object Strip {

  implicit val stripComonad: StripComonad = new StripComonad

  private[types] class StripComonad extends Comonad[Strip] {

    final override def extract[A](x: Strip[A]): A = x.extract
    final override def coflatMap[A, B](fa: Strip[A])(f: Strip[A] => B): Strip[B] = fa.coflatMap(f)
    final override def map[A, B](fa: Strip[A])(f: A => B): Strip[B] = coflatMap(fa)(x => f(extract(x)))
  }
}
