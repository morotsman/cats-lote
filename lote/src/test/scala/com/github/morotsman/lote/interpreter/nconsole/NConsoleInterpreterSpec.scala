package com.github.morotsman.lote.interpreter.nconsole

import cats.effect.IO
import com.github.morotsman.lote.model._
import munit.CatsEffectSuite

import scala.collection.mutable.ListBuffer

class NConsoleInterpreterSpec extends CatsEffectSuite {

  /** A fake Terminal that returns pre-configured characters from read() */
  class FakeTerminal(
                      inputs: List[Int],
                      val width: Int = 40,
                      val height: Int = 10
                    ) extends Terminal {
    private val inputQueue = inputs.iterator
    private val written = ListBuffer.empty[String]
    private var flushCount = 0

    override def read(timeoutInMillis: Long): Int =
      if (inputQueue.hasNext) inputQueue.next() else 65534 // timeout

    override def flush(): Unit = flushCount += 1
    override def write(s: String): Unit = written += s

    def getWritten: List[String] = written.toList
    def getFlushCount: Int = flushCount
  }

  test("read returns Character for normal input") {
    val terminal = new FakeTerminal(inputs = List('a'.toInt))
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      result <- console.read()
    } yield assertEquals(result, Character('a'))
  }

  test("read returns Key(Right) for escape sequence") {
    // ESC [ C = Right arrow
    val terminal = new FakeTerminal(inputs = List(27, '['.toInt, 'C'.toInt))
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      result <- console.read()
    } yield assertEquals(result, Key(SpecialKey.Right))
  }

  test("read returns Key(Left) for escape sequence") {
    val terminal = new FakeTerminal(inputs = List(27, '['.toInt, 'D'.toInt))
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      result <- console.read()
    } yield assertEquals(result, Key(SpecialKey.Left))
  }

  test("read returns Key(Up) for escape sequence") {
    val terminal = new FakeTerminal(inputs = List(27, '['.toInt, 'A'.toInt))
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      result <- console.read()
    } yield assertEquals(result, Key(SpecialKey.Up))
  }

  test("read returns Key(Down) for escape sequence") {
    val terminal = new FakeTerminal(inputs = List(27, '['.toInt, 'B'.toInt))
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      result <- console.read()
    } yield assertEquals(result, Key(SpecialKey.Down))
  }

  test("read returns Key(Esc) for bare escape") {
    // ESC followed by non-'[' character
    val terminal = new FakeTerminal(inputs = List(27, 'x'.toInt))
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      result <- console.read()
    } yield assertEquals(result, Key(SpecialKey.Esc))
  }

  test("read returns Key(Unknown) for unrecognized escape sequence") {
    val terminal = new FakeTerminal(inputs = List(27, '['.toInt, 'Z'.toInt))
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      result <- console.read()
    } yield assertEquals(result, Key(SpecialKey.Unknown))
  }

  test("read returns Key(Timeout) for 65534") {
    val terminal = new FakeTerminal(inputs = List(65534))
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      result <- console.read()
    } yield assertEquals(result, Key(SpecialKey.Timeout))
  }

  test("alignText aligns content and returns ScreenAdjusted") {
    val terminal = new FakeTerminal(inputs = Nil, width = 20, height = 5)
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      result <- console.alignText("Hi", Alignment(VerticalAlignment.Center, HorizontalAlignment.Center))
    } yield {
      assert(result.content.contains("Hi"))
      val lines = result.content.split("\n")
      lines.foreach(l => assertEquals(l.length, 20))
    }
  }

  test("alignText truncates lines that exceed terminal width") {
    val terminal = new FakeTerminal(inputs = Nil, width = 5, height = 4)
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      result <- console.alignText("ABCDEFGHIJ", Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
    } yield {
      val lines = result.content.split("\n")
      // Content should be truncated to width=5
      assert(lines(0).length == 5)
      assert(!lines(0).contains("FGHIJ"))
    }
  }

  test("alignText truncates rows that exceed terminal height") {
    val terminal = new FakeTerminal(inputs = Nil, width = 10, height = 3)
    val console = NConsoleInterpreter.make[IO](terminal)
    val longContent = (1 to 10).map(i => s"Line$i").mkString("\n")
    for {
      result <- console.alignText(longContent, Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
    } yield {
      val lines = result.content.split("\n")
      // height - 1 = 2 lines of content max
      assert(lines.length <= 3)
    }
  }

  test("writeString delegates to terminal.write") {
    val terminal = new FakeTerminal(inputs = Nil)
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      _ <- console.writeString(ScreenAdjusted("hello world"))
    } yield {
      assertEquals(terminal.getWritten, List("hello world"))
    }
  }

  test("clear delegates to terminal.flush") {
    val terminal = new FakeTerminal(inputs = Nil)
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      _ <- console.clear()
    } yield {
      assertEquals(terminal.getFlushCount, 1)
    }
  }

  test("context returns Screen with terminal dimensions") {
    val terminal = new FakeTerminal(inputs = Nil, width = 80, height = 24)
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      screen <- console.context
    } yield {
      assertEquals(screen, Screen(80, 24))
    }
  }

  test("readInterruptible returns first non-timeout input") {
    // First two reads timeout, third is a real key
    val terminal = new FakeTerminal(inputs = List(65534, 65534, 'x'.toInt))
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      result <- console.readInterruptible()
    } yield {
      assertEquals(result, Character('x'))
    }
  }

  test("read returns MouseClick for SGR mouse click sequence") {
    // ESC [ < 0 ; 10 ; 20 M  (button 0 = left click at x=10, y=20, 'M' = press)
    val inputs = List(27, '['.toInt, '<'.toInt) ++
      "0;10;20".map(_.toInt).toList ++
      List('M'.toInt)
    val terminal = new FakeTerminal(inputs = inputs)
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      result <- console.read()
    } yield assertEquals(result, MouseClick(10, 20))
  }

  test("read returns MouseClick for right mouse button") {
    // ESC [ < 2 ; 5 ; 8 M  (button 2 = right click)
    val inputs = List(27, '['.toInt, '<'.toInt) ++
      "2;5;8".map(_.toInt).toList ++
      List('M'.toInt)
    val terminal = new FakeTerminal(inputs = inputs)
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      result <- console.read()
    } yield assertEquals(result, MouseClick(5, 8))
  }

  test("read returns MouseMove for SGR mouse move sequence") {
    // ESC [ < 35 ; 15 ; 25 M  (button 35 = motion with no button, bit 5 set)
    val inputs = List(27, '['.toInt, '<'.toInt) ++
      "35;15;25".map(_.toInt).toList ++
      List('M'.toInt)
    val terminal = new FakeTerminal(inputs = inputs)
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      result <- console.read()
    } yield assertEquals(result, MouseMove(15, 25))
  }

  test("read returns MouseMove for motion with left button held") {
    // ESC [ < 32 ; 3 ; 7 M  (button 32 = motion + left button)
    val inputs = List(27, '['.toInt, '<'.toInt) ++
      "32;3;7".map(_.toInt).toList ++
      List('M'.toInt)
    val terminal = new FakeTerminal(inputs = inputs)
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      result <- console.read()
    } yield assertEquals(result, MouseMove(3, 7))
  }

  test("read returns Unknown for mouse button release") {
    // ESC [ < 0 ; 10 ; 20 m  (lowercase 'm' = release)
    val inputs = List(27, '['.toInt, '<'.toInt) ++
      "0;10;20".map(_.toInt).toList ++
      List('m'.toInt)
    val terminal = new FakeTerminal(inputs = inputs)
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      result <- console.read()
    } yield assertEquals(result, Key(SpecialKey.Unknown))
  }

  test("close calls terminal.close") {
    var closed = false
    val terminal = new FakeTerminal(inputs = Nil) {
      override def close(): Unit = closed = true
    }
    val console = NConsoleInterpreter.make[IO](terminal)
    for {
      _ <- console.close()
    } yield assert(closed, "terminal.close() should have been called")
  }
}

