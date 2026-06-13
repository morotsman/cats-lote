package com.github.morotsman.lote.interpreter.nconsole

import org.jline.terminal.TerminalBuilder
import org.jline.utils.InfoCmp.Capability

/**
 * JLine-based Terminal implementation. Initializes raw mode and clears the screen.
 */
object JLineTerminal {
  def make(): Terminal = {
    val jlineTerminal = TerminalBuilder.terminal()
    val reader = jlineTerminal.reader()
    jlineTerminal.enterRawMode()
    jlineTerminal.puts(Capability.clear_screen)

    // Enable mouse tracking: X10 compatibility mode + any-event tracking + SGR extended coordinates
    val writer = jlineTerminal.writer()
    writer.print("\u001b[?1000h") // enable basic mouse click reporting
    writer.print("\u001b[?1003h") // enable any-event (movement) tracking
    writer.print("\u001b[?1006h") // enable SGR extended mouse mode
    writer.flush()

    new Terminal {
      override def read(timeoutInMillis: Long): Int = reader.read(timeoutInMillis)
      override val width: Int = jlineTerminal.getWidth
      override val height: Int = jlineTerminal.getHeight
      override def flush(): Unit = jlineTerminal.flush()
      override def write(s: String): Unit = {
        print("\u001b[H")
        println(s)
      }
      override def close(): Unit = {
        // Disable mouse tracking before closing
        val w = jlineTerminal.writer()
        w.print("\u001b[?1003l")
        w.print("\u001b[?1000l")
        w.print("\u001b[?1006l")
        w.flush()
        jlineTerminal.close()
      }
    }
  }
}

