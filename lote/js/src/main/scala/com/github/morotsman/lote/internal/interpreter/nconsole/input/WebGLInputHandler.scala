package com.github.morotsman.lote.internal.interpreter.nconsole.input

import cats.effect.std.{Dispatcher, Queue}
import com.github.morotsman.lote.internal.interpreter.nconsole.KeyMapper
import org.scalajs.dom.{ClipboardEvent, Event, HTMLCanvasElement, KeyboardEvent, MouseEvent}

/** Bridges DOM input events (keyboard, mouse, paste) into an effectful `Queue[F, Int]`.
  *
  * Produces the same character/escape sequences as JLine and xterm.js so the rest of the framework can handle input
  * uniformly across all backends.
  */
private[nconsole] object WebGLInputHandler {

  /** Wire up keyboard, mouse, and paste listeners on the given canvas element.
    *
    * @return
    *   a reference to the keydown listener so it can be removed on close
    */
  def attach[F[_]](
      canvas: HTMLCanvasElement,
      queue: Queue[F, Int],
      dispatcher: Dispatcher[F],
      cellWidth: Int,
      cellHeight: Int
  ): scalajs.js.Function1[KeyboardEvent, Unit] = {

    // Make focusable
    canvas.tabIndex = 0
    canvas.style.outline = "none"
    canvas.focus()

    // ---- Keyboard ----
    val keyListener: scalajs.js.Function1[KeyboardEvent, Unit] = { (ev: KeyboardEvent) =>
      keyEventToChars(ev).foreach { c =>
        dispatcher.unsafeRunAndForget(queue.offer(c))
      }
    }
    canvas.addEventListener("keydown", keyListener)

    // ---- Paste ----
    canvas.addEventListener(
      "paste",
      { (ev: Event) =>
        val text = ev.asInstanceOf[ClipboardEvent].clipboardData.getData("text/plain")
        text.foreach { c =>
          dispatcher.unsafeRunAndForget(queue.offer(c.toInt))
        }
      }
    )

    // ---- Mouse down ----
    canvas.addEventListener(
      "mousedown",
      { (ev: MouseEvent) =>
        val rect = canvas.getBoundingClientRect()
        val col = ((ev.clientX - rect.left) / cellWidth).toInt + 1
        val row = ((ev.clientY - rect.top) / cellHeight).toInt + 1
        val button = ev.button.toInt
        s"\u001b[<$button;$col;${row}M".foreach(c => dispatcher.unsafeRunAndForget(queue.offer(c.toInt)))
      }: scalajs.js.Function1[MouseEvent, Unit]
    )

    // ---- Mouse move ----
    canvas.addEventListener(
      "mousemove",
      { (ev: MouseEvent) =>
        val rect = canvas.getBoundingClientRect()
        val col = ((ev.clientX - rect.left) / cellWidth).toInt + 1
        val row = ((ev.clientY - rect.top) / cellHeight).toInt + 1
        s"\u001b[<35;$col;${row}M".foreach(c => dispatcher.unsafeRunAndForget(queue.offer(c.toInt)))
      }: scalajs.js.Function1[MouseEvent, Unit]
    )

    // ---- Mouse up ----
    canvas.addEventListener(
      "mouseup",
      { (ev: MouseEvent) =>
        val rect = canvas.getBoundingClientRect()
        val col = ((ev.clientX - rect.left) / cellWidth).toInt + 1
        val row = ((ev.clientY - rect.top) / cellHeight).toInt + 1
        val button = ev.button.toInt
        s"\u001b[<$button;$col;${row}m".foreach(c => dispatcher.unsafeRunAndForget(queue.offer(c.toInt)))
      }: scalajs.js.Function1[MouseEvent, Unit]
    )

    keyListener
  }

  /** Convert a DOM KeyboardEvent to character codes matching the encoding used by JLine/xterm.js.
    *
    * Delegates to [[KeyMapper.keyNameToChars]] for the pure mapping logic, then calls `preventDefault()` on the event
    * if any codes were produced (to suppress browser default actions for navigation keys).
    */
  private def keyEventToChars(ev: KeyboardEvent): Seq[Int] = {
    val result = KeyMapper.keyNameToChars(ev.key, ev.ctrlKey)
    if (result.nonEmpty) ev.preventDefault()
    result
  }
}
