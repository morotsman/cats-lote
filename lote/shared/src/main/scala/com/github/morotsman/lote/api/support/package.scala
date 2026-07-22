package com.github.morotsman.lote.api

package object support {

  /** @deprecated Use [[AnimationClock]] instead. Renamed to avoid confusion with `cats.effect.Clock`. */
  @deprecated("Use AnimationClock instead", "0.1.0")
  type Clock[F[_]] = AnimationClock[F]

  /** @deprecated Use [[AnimationClock]] companion instead. Renamed to avoid confusion with `cats.effect.Clock`. */
  @deprecated("Use AnimationClock instead", "0.1.0")
  val Clock: AnimationClock.type = AnimationClock
}
