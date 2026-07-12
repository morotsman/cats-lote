package com.github.morotsman.lote.api

import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** Controls how quickly built-in animations advance.
  *
  * This is separate from the ticker interval: the ticker controls render/update cadence, while `step` controls the
  * simulation speed of built-in transitions and animations.
  */
final case class AnimationSettings(step: FiniteDuration)

object AnimationSettings {
  val DefaultStep: FiniteDuration = 40.millis

  implicit val default: AnimationSettings = AnimationSettings(DefaultStep)
}

