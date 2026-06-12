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

    new Terminal {
      override def read(timeoutInMillis: Long): Int = reader.read(timeoutInMillis)
      override val width: Int = jlineTerminal.getWidth
      override val height: Int = jlineTerminal.getHeight
      override def flush(): Unit = jlineTerminal.flush()
      override def write(s: String): Unit = {
        print("\u001b[H")
        println(s)
      }
    }
  }
}

