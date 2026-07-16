package com.github.morotsman.lote.internal.interpreter.nconsole

import cats.effect.{Resource, Sync}
import com.github.morotsman.lote.api.Screen
import com.github.morotsman.lote.api.spi.{Terminal => TerminalAlgebra}
import org.jline.terminal.TerminalBuilder
import org.jline.utils.InfoCmp.Capability
import sun.misc.{Signal, SignalHandler}

/** JLine-based Terminal implementation. Initializes raw mode, enables mouse tracking, and clears the screen.
  */
private[lote] object JLineTerminal {

  /** Creates a JLine-backed `Terminal[F]` as a `Resource` that automatically restores terminal state on release. */
  def resource[F[_]: Sync](): Resource[F, TerminalAlgebra[F]] =
    Resource.make(Sync[F].blocking(createTerminal[F]()))(_.close())

  private def createTerminal[F[_]: Sync](): TerminalAlgebra[F] = {
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

    new TerminalAlgebra[F] {
      override def read(timeoutInMillis: Long): F[Int] =
        Sync[F].blocking(reader.read(timeoutInMillis))

      override def size: F[Screen] =
        Sync[F].blocking(Screen(screenWidth = jlineTerminal.getWidth, screenHeight = jlineTerminal.getHeight))

      override def write(s: String): F[Unit] =
        Sync[F].blocking {
          renderLock.synchronized {
            val (nextFrame, command): (Vector[String], String) =
              AnsiFrameRenderer.render(previousFrame, s, jlineTerminal.getWidth, jlineTerminal.getHeight)
            previousFrame = nextFrame
            if (command.nonEmpty) {
              writer.print(command)
              writer.flush()
            }
          }
        }

      override def flush(): F[Unit] =
        Sync[F].blocking(jlineTerminal.flush())

      override def close(): F[Unit] =
        Sync[F].blocking {
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
