package coscala.types

import cats.Comonad

case class Strip[A](as: Seq[A], index: Int) {
  assert(index >= 0)
  assert(index < as.length)

  def extract: A = as(index)

  def coflatMap[B](f: Strip[A] => B): Strip[B] = Strip(as.indices.map(i => f(Strip(as, i))), index)

  def shiftLeft: Strip[A] = {
    val newIndex = index - 1
    Strip(as, if (newIndex < 0) as.length - 1 else newIndex)
  }

  def shiftRight: Strip[A] = {
    val newIndex = index + 1
    Strip(as, if (newIndex >= as.length) 0 else newIndex)
  }
}

object Strip {

  implicit val stripComonad: StripComonad = new StripComonad

  private[types] class StripComonad extends Comonad[Strip] {

    final override def extract[A](x: Strip[A]): A = x.extract
    final override def coflatMap[A, B](fa: Strip[A])(f: Strip[A] => B): Strip[B] = fa.coflatMap(f)
    final override def map[A, B](fa: Strip[A])(f: A => B): Strip[B] = coflatMap(fa)(x => f(extract(x)))
  }
}
