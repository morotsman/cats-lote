package com.github.morotsman.lote.internal.interpreter.nconsole

import org.jline.terminal.TerminalBuilder
import org.jline.utils.InfoCmp.Capability
import sun.misc.{Signal, SignalHandler}

/** JLine-based Terminal implementation. Initializes raw mode and clears the screen.
  */
private[lote] object JLineTerminal {
  def make(): Terminal = {
    val jlineTerminal = TerminalBuilder.terminal()
    val reader = jlineTerminal.reader()
    jlineTerminal.enterRawMode()
    jlineTerminal.puts(Capability.clear_screen)
    jlineTerminal.writer().print("\u001b[?25l")

    // Enable mouse tracking: X10 compatibility mode + any-event tracking + SGR extended coordinates
    val writer = jlineTerminal.writer()
    writer.print("\u001b[?1000h") // enable basic mouse click reporting
    writer.print("\u001b[?1003h") // enable any-event (movement) tracking
    writer.print("\u001b[?1006h") // enable SGR extended mouse mode
    writer.flush()

    def disableMouseTracking(): Unit = {
      try {
        val out = System.out
        out.print("\u001b[?25h")
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
    prevHandler = Signal.handle(
      new Signal("INT"),
      new SignalHandler {
        override def handle(sig: Signal): Unit = {
          disableMouseTracking()
          // Chain to the previous handler so the application/sbt can terminate gracefully.
          // Never call System.exit() here, it triggers JVM shutdown which conflicts with
          // sbt's own shutdown hook management.
          if (prevHandler != null && prevHandler != SignalHandler.SIG_DFL && prevHandler != SignalHandler.SIG_IGN) {
            prevHandler.handle(sig)
          } else {
            // No meaningful previous handler, restore default and re-raise so the OS
            // delivers the default SIGINT behavior (process termination) without going
            // through System.exit().
            Signal.handle(new Signal("INT"), SignalHandler.SIG_DFL)
            Signal.raise(new Signal("INT"))
          }
        }
      }
    )

    // Also keep the shutdown hook as a fallback for non-SIGINT exits
    val shutdownHook = new Thread(() => {
      disableMouseTracking()
      try { jlineTerminal.close() }
      catch { case _: Throwable => }
    })
    Runtime.getRuntime.addShutdownHook(shutdownHook)

    val renderLock = new AnyRef
    var previousFrame = Vector.empty[String]

    new Terminal {
      override def read(timeoutInMillis: Long): Int =
        reader.read(timeoutInMillis)
      override val width: Int = jlineTerminal.getWidth
      override val height: Int = jlineTerminal.getHeight
      override def flush(): Unit = jlineTerminal.flush()
      override def write(s: String): Unit = {
        renderLock.synchronized {
          val (nextFrame, command): (Vector[String], String) =
            com.github.morotsman.lote.internal.interpreter.nconsole.AnsiFrameRenderer
              .render(previousFrame, s, width, height)
          previousFrame = nextFrame
          if (command.nonEmpty) {
            writer.print(command)
            writer.flush()
          }
        }
      }
      override def close(): Unit = {
        // Restore previous signal handler
        try { Signal.handle(new Signal("INT"), prevHandler) }
        catch { case _: Throwable => }
        // Remove shutdown hook since we're closing cleanly
        try { Runtime.getRuntime.removeShutdownHook(shutdownHook) }
        catch { case _: IllegalStateException => }
        // Disable mouse tracking before closing
        val w = jlineTerminal.writer()
        w.print("\u001b[?25h")
        w.print("\u001b[?1003l")
        w.print("\u001b[?1000l")
        w.print("\u001b[?1006l")
        w.flush()
        jlineTerminal.close()
      }
    }
  }
}

