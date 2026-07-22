package com.github.morotsman.lote.api.spi

import com.github.morotsman.lote.api.{PlatformCapability, Screen}

import scala.annotation.implicitNotFound

/** Low-level terminal backend abstraction.
  *
  * Provides raw I/O operations (read characters, write strings, query size) without any protocol-level parsing.
  * `NConsoleInterpreter` wraps a `Terminal[F]` to produce a full `NConsole[F]` with input parsing, text alignment, and
  * lifecycle management.
  *
  * Implement this trait to plug in an alternative backend (e.g., a web-based terminal or an in-memory test terminal)
  * without changing any presentation logic.
  *
  * The built-in JLine implementation is available via `JLineTerminal.resource[F]()`.
  */
@implicitNotFound(
  "No implicit Terminal[${F}] found. Use JLineTerminal.resource() for a JLine-backed terminal, or provide your own Terminal[${F}] implementation for alternative backends (e.g., web-based)."
)
trait Terminal[F[_]] {

  /** Read a single character code from the terminal.
    *
    * @param timeoutInMillis
    *   maximum time to wait; 0 means blocking read
    * @return
    *   the character code, or 65534 on timeout
    */
  def read(timeoutInMillis: Long): F[Int]

  /** Returns the current terminal dimensions. */
  def size: F[Screen]

  /** Write a raw string to the terminal output. */
  def write(s: String): F[Unit]

  /** Write a raw ANSI escape string directly to the terminal output,
    * bypassing any frame-diffing or buffering logic.
    *
    * The default implementation delegates to `write`, which is correct for
    * backends that do not perform differential rendering. Backends that use
    * frame-diffing (e.g. JLineTerminal) should override this to write
    * directly to the underlying output stream.
    */
  def writeRaw(s: String): F[Unit] = write(s)

  /** Flush the terminal output buffer. */
  def flush(): F[Unit]

  /** Release terminal resources (restore state, close handles). */
  def close(): F[Unit]

  /** Advertises what this terminal backend can render.
    *
    * Override in backends that support richer rendering (sub-pixel positioning, effects, 3D transforms). The default is
    * `CharacterGrid` only, which is appropriate for JLine and xterm.js terminals.
    */
  def capabilities: Set[PlatformCapability] = Set(PlatformCapability.CharacterGrid)

  /** Returns a reference to the shared 3D scene, if the backend supports spatial mode.
    *
    * On WebGL backends in spatial mode, this returns `Some(Scene3DRef)` which can be cast to
    * `com.github.morotsman.lote.api.Scene3DRef` for adding 3D geometry to the shared scene. On terminal backends this
    * returns `None`.
    */
  def scene3DRef: Option[Any] = None
}
