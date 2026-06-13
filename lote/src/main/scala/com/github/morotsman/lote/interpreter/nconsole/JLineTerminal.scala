package com.github.morotsman.lote.interpreter.nconsole

import org.jline.terminal.TerminalBuilder
import org.jline.utils.InfoCmp.Capability
import sun.misc.{Signal, SignalHandler}

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

    def disableMouseTracking(): Unit = {
      try {
        val out = System.out
        out.print("\u001b[?1003l")
        out.print("\u001b[?1000l")
        out.print("\u001b[?1006l")
        out.flush()
      } catch {
        case _: Throwable => // best effort
      }
    }

    // Register SIGINT handler to disable mouse tracking immediately when Ctrl+C is pressed.
    // This fires even when running inside sbt (which doesn't exit the JVM on Ctrl+C).
    @volatile var prevHandler: SignalHandler = null
    prevHandler = Signal.handle(new Signal("INT"), new SignalHandler {
      override def handle(sig: Signal): Unit = {
        disableMouseTracking()
        // Chain to the previous handler so the application actually terminates
        if (prevHandler == SignalHandler.SIG_DFL) {
          // Default behavior: exit the process
          System.exit(130) // 128 + SIGINT(2)
        } else if (prevHandler != null && prevHandler != SignalHandler.SIG_IGN) {
          prevHandler.handle(sig)
        } else {
          // No previous handler or ignored - force exit
          System.exit(130)
        }
      }
    })

    // Also keep the shutdown hook as a fallback for non-SIGINT exits
    val shutdownHook = new Thread(() => {
      disableMouseTracking()
      try { jlineTerminal.close() } catch { case _: Throwable => }
    })
    Runtime.getRuntime.addShutdownHook(shutdownHook)

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
        // Restore previous signal handler
        try { Signal.handle(new Signal("INT"), prevHandler) } catch { case _: Throwable => }
        // Remove shutdown hook since we're closing cleanly
        try { Runtime.getRuntime.removeShutdownHook(shutdownHook) } catch { case _: IllegalStateException => }
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

