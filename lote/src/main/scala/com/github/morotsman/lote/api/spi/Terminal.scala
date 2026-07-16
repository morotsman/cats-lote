package com.github.morotsman.lote.api.spi

import com.github.morotsman.lote.api.Screen

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
@implicitNotFound("No implicit Terminal[${F}] found. Use JLineTerminal.resource() for a JLine-backed terminal, or provide your own Terminal[${F}] implementation for alternative backends (e.g., web-based).")
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

  /** Flush the terminal output buffer. */
  def flush(): F[Unit]

  /** Release terminal resources (restore state, close handles). */
  def close(): F[Unit]
}

