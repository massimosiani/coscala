package coscala

import cats.implicits._
import coscala.types.{Pairing, Strip}
import Pairing.StateStorePairing
import Instances._

object Coscala extends App {

  private val size = 30
  private val iterations = 90
  private val start: Strip[Boolean] = Strip(
    (IndexedSeq.fill(size)(false) :+ true) ++ IndexedSeq.fill(size)(false),
    0
  )
  private val actualStore = storeFrom(start)
  private val solution = select(stateAfter(iterations))(actualStore.coflatten)
  println(solution.extract)
}
