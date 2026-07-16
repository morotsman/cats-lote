package com.github.morotsman.lote.internal.interpreter.nconsole

import cats.effect.std.{Dispatcher, Queue}
import cats.effect.{Async, Resource, Sync}
import com.github.morotsman.lote.api.Screen
import com.github.morotsman.lote.api.spi.{Terminal => TerminalAlgebra}
import org.scalajs.dom

import scala.concurrent.duration._
import cats.syntax.flatMap._
import cats.syntax.functor._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

import scala.annotation.nowarn

/** Scala.js facade for the xterm.js Terminal class.
  *
  * xterm.js is expected to be available as a global (`window.Terminal`) when loaded via CDN, or as an ES module import
  * when bundled via npm.
  */
@js.native
@JSGlobal("Terminal")
@nowarn("cat=unused")
class XtermJs(options: js.UndefOr[js.Object]) extends js.Object {
  def write(data: String): Unit = js.native
  def cols: Int = js.native
  def rows: Int = js.native
  def onData(callback: js.Function1[String, Unit]): js.Object = js.native
  def onResize(callback: js.Function1[js.Dynamic, Unit]): js.Object = js.native
  def open(element: dom.HTMLElement): Unit = js.native
  def dispose(): Unit = js.native
  def focus(): Unit = js.native
  def loadAddon(addon: js.Any): Unit = js.native
}

/** Scala.js facade for the xterm.js FitAddon (xterm-addon-fit). */
@js.native
@JSGlobal("FitAddon.FitAddon")
@nowarn("cat=unused")
class XtermFitAddon() extends js.Object {
  def fit(): Unit = js.native
  def proposeDimensions(): js.Dynamic = js.native
}

/** Browser-backed `Terminal[F]` implementation using xterm.js.
  *
  * xterm.js natively interprets the same ANSI escape sequences that the JLine backend emits, so no translation is
  * needed — strings are passed through directly.
  *
  * Input characters from `onData` are bridged into the effect system via a `Queue` and a `Dispatcher`.
  */
object XtermTerminal {

  /** Creates an xterm.js-backed `Terminal[F]` as a `Resource` that disposes the xterm.js instance on release. */
  def resource[F[_]: Async](container: dom.HTMLElement): Resource[F, TerminalAlgebra[F]] =
    for {
      dispatcher <- Dispatcher.sequential[F]
      terminal   <- Resource.make(create[F](container, dispatcher))(_.close())
    } yield terminal

  private def create[F[_]: Async](
      container: dom.HTMLElement,
      dispatcher: Dispatcher[F]
  ): F[TerminalAlgebra[F]] = {
    val F = Async[F]
    for {
      queue <- Queue.unbounded[F, Int]
      xterm <- Sync[F].delay {
        val opts = js.Dynamic.literal(
          cursorBlink = false,
          disableStdin = false,
          convertEol = true
        )
        val t = new XtermJs(opts.asInstanceOf[js.UndefOr[js.Object]])

        // Load the fit addon so the terminal fills its container
        val fitAddon = new XtermFitAddon()
        t.loadAddon(fitAddon)

        t.open(container)

        // Fit to container size immediately
        fitAddon.fit()

        // Re-fit whenever the browser window is resized
        dom.window.addEventListener("resize", (_: dom.Event) => fitAddon.fit())

        // Hide the cursor (presentation mode)
        t.write("\u001b[?25l")

        // Enable mouse tracking (same sequences as JLine)
        t.write("\u001b[?1000h") // basic mouse click reporting
        t.write("\u001b[?1003h") // any-event (movement) tracking
        t.write("\u001b[?1006h") // SGR extended mouse mode

        t.focus()

        // Bridge xterm.js onData callback into the effectful Queue via Dispatcher
        t.onData { (data: String) =>
          data.foreach { c =>
            dispatcher.unsafeRunAndForget(queue.offer(c.toInt))
          }
        }

        t
      }
    } yield new TerminalAlgebra[F] {

      private var previousFrame = Vector.empty[String]
      @volatile private var currentCols: Int = xterm.cols
      @volatile private var currentRows: Int = xterm.rows

      // Track resize events
      xterm.onResize { (event: js.Dynamic) =>
        currentCols = event.cols.asInstanceOf[Int]
        currentRows = event.rows.asInstanceOf[Int]
      }

      override def read(timeoutInMillis: Long): F[Int] =
        if (timeoutInMillis <= 0)
          queue.take
        else
          F.timeoutTo(
            queue.take,
            timeoutInMillis.millis,
            F.pure(65534) // timeout sentinel, same as JLine
          )

      override def size: F[Screen] =
        Sync[F].delay(Screen(screenWidth = currentCols, screenHeight = currentRows))

      override def write(s: String): F[Unit] =
        Sync[F].delay {
          val (nextFrame, command): (Vector[String], String) =
            AnsiFrameRenderer.render(previousFrame, s, currentCols, currentRows)
          previousFrame = nextFrame
          if (command.nonEmpty) {
            xterm.write(command)
          }
        }

      override def flush(): F[Unit] =
        F.unit // xterm.js renders immediately

      override def close(): F[Unit] =
        Sync[F].delay {
          // Disable mouse tracking
          xterm.write("\u001b[?25h")
          xterm.write("\u001b[?1003l")
          xterm.write("\u001b[?1000l")
          xterm.write("\u001b[?1006l")
          xterm.dispose()
        }
    }
  }
}






