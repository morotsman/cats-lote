package com.github.morotsman.lote.api.spi

import com.github.morotsman.lote.api.RenderEffect

/** Extended terminal that supports GPU-accelerated visual effects.
  *
  * Backends like `ThreeJsTerminal` mix in this trait to handle `RenderEffect` commands (3D flips, dissolve, smoke,
  * glow, etc.). Terminal-only backends (JLine, xterm.js) do not implement this trait — `NConsole.applyEffect` becomes a
  * no-op for them.
  *
  * Transitions check `NConsole.capabilities` to decide whether to use effects, so the same transition code works on all
  * platforms.
  */
trait EffectfulTerminal[F[_]] { self: Terminal[F] =>

  /** Apply a visual effect to the current rendering.
    *
    * Effects are cumulative until `RenderEffect.ClearEffects` is applied. The terminal renders the effect on the next
    * frame.
    */
  def applyEffect(effect: RenderEffect): F[Unit]
}
