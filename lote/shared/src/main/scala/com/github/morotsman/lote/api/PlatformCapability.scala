package com.github.morotsman.lote.api

/** Describes what a terminal backend is capable of rendering.
  *
  * Terminal-based backends (JLine, xterm.js) are limited to `CharacterGrid` — the smallest unit of movement is one
  * character cell. WebGL-backed terminals can advertise richer capabilities like sub-pixel rendering and visual
  * effects, enabling transitions to produce smoother animations and GPU-accelerated visuals.
  *
  * Transitions can query `NConsole.capabilities` (or `SlideContext.capabilities`) to decide whether to use a plain
  * character-grid animation or a richer visual effect.
  */
sealed trait PlatformCapability

object PlatformCapability {

  /** Character-grid rendering only. Movement is cell-to-cell (one character width/height per step). All backends
    * support this.
    */
  case object CharacterGrid extends PlatformCapability

  /** Sub-pixel positioning is available. Characters can be rendered at fractional pixel positions, enabling smoother
    * motion than cell-to-cell jumps.
    */
  case object SubPixelRendering extends PlatformCapability

  /** Visual effects (dissolve, smoke, glow, etc.) are supported via `NConsole.applyEffect`. */
  case object Effects extends PlatformCapability

  /** 3D transforms (flip, rotate, camera movement) are supported via `NConsole.applyEffect`. */
  case object Transforms3D extends PlatformCapability
}
