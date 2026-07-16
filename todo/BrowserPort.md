# Porting cats-lote to the Browser

## Overview

The goal is to make cats-lote run in a web browser in addition to the terminal. The existing `Terminal[F[_]]` SPI trait is the abstraction boundary — we create a new browser-backed implementation while keeping all presentation logic unchanged.

## Technology Stack

| Layer | Technology | Notes |
|-------|-----------|-------|
| Compiler | **Scala.js** | Compiles Scala 2.13 to JavaScript |
| Effect runtime | **Cats Effect (Scala.js build)** | Full IO support on JS |
| Terminal emulator | **xterm.js** | Renders ANSI sequences in the browser (same lib VS Code uses) |
| Build plugin | **sbt-scalajs** + **sbt-crossproject** | Enables JVM/JS cross-compilation |

## Why xterm.js?

The library already emits ANSI escape codes for cursor movement, colors, clearing, etc. xterm.js interprets these natively, so `write(s)` in the browser is essentially just forwarding the string to xterm.js — no custom rendering logic needed.

---

## Architecture Changes

### Current structure

```
lote/src/main/scala/
  com/github/morotsman/lote/
    api/spi/Terminal.scala          ← trait Terminal[F[_]]
    internal/interpreter/nconsole/
      JLineTerminal.scala           ← JLine-backed impl (JVM only)
```

### Proposed structure (cross-project)

```
lote/
  shared/src/main/scala/           ← all existing code (pure Cats Effect)
    com/github/morotsman/lote/...
  jvm/src/main/scala/              ← JLine terminal (existing)
    com/github/morotsman/lote/internal/interpreter/nconsole/JLineTerminal.scala
  js/src/main/scala/               ← new browser terminal
    com/github/morotsman/lote/internal/interpreter/nconsole/XtermTerminal.scala
```

---

## Build Changes

### project/plugins.sbt

```scala
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.17.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
```

### build.sbt (sketch)

```scala
import sbtcrossproject.CrossPlugin.autoImport._

lazy val lote = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("lote"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core"       % "2.12.0",
      "org.typelevel" %%% "cats-effect"     % "3.5.7",
      // test
      "org.typelevel" %%% "munit-cats-effect" % "2.0.0" % Test,
      "org.scalameta" %%% "munit"            % "1.0.3" % Test,
    ),
    testFrameworks += new TestFramework("munit.Framework"),
  )
  .jvmSettings(
    libraryDependencies += "org.jline" % "jline" % "3.27.1"
  )
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    // xterm.js is loaded externally via npm/CDN, accessed through a facade
  )

lazy val loteJVM = lote.jvm
lazy val loteJS  = lote.js
```

---

## XtermTerminal Implementation (Sketch)

```scala
package com.github.morotsman.lote.internal.interpreter.nconsole

import cats.effect.{IO, Async, Sync}
import cats.effect.std.Queue
import com.github.morotsman.lote.api.Screen
import com.github.morotsman.lote.api.spi.Terminal
import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

// Scala.js facade for xterm.js
@js.native
@JSImport("xterm", "Terminal")
class XtermJs(options: js.UndefOr[js.Object] = js.undefined) extends js.Object {
  def write(data: String): Unit = js.native
  def cols: Int = js.native
  def rows: Int = js.native
  def onData(callback: js.Function1[String, Unit]): Unit = js.native
  def open(element: dom.HTMLElement): Unit = js.native
  def dispose(): Unit = js.native
}

object XtermTerminal {

  def apply[F[_]: Async](container: dom.HTMLElement): F[Terminal[F]] =
    for {
      queue <- Queue.unbounded[F, Int]
      xterm <- Sync[F].delay {
        val t = new XtermJs()
        t.open(container)
        t.onData { (data: String) =>
          // Enqueue each character code
          data.foreach(ch => 
            // Fire-and-forget into the queue (unsafe in real code, 
            // use Dispatcher for proper F ~> sync bridge)
            ()
          )
        }
        t
      }
    } yield new Terminal[F] {

      def read(timeoutInMillis: Long): F[Int] =
        if (timeoutInMillis == 0)
          queue.take
        else
          Async[F].timeoutTo(
            queue.take,
            scala.concurrent.duration.FiniteDuration(timeoutInMillis, "ms"),
            Async[F].pure(65534)
          )

      def size: F[Screen] =
        Sync[F].delay(Screen(xterm.rows, xterm.cols))

      def write(s: String): F[Unit] =
        Sync[F].delay(xterm.write(s))

      def flush(): F[Unit] =
        Sync[F].unit  // xterm.js renders immediately

      def close(): F[Unit] =
        Sync[F].delay(xterm.dispose())
    }
}
```

### Key Implementation Notes

- **Input handling**: Use a `Dispatcher[F]` to bridge between the synchronous xterm.js `onData` callback and the effectful `Queue`. The sketch above omits this for brevity.
- **Timeout on read**: Use `Async[F].timeoutTo` to replicate the timeout behavior of the JLine version.
- **Screen size**: xterm.js exposes `cols` and `rows` directly.
- **ANSI pass-through**: No translation needed — xterm.js handles the same escape sequences JLine does.

---

## Client Usage (Published Library)

Assuming the library is published to Maven Central as:
- `com.github.morotsman` %%% `cats-lote` % `<version>`

### JVM Client (Terminal App — unchanged)

```scala
// build.sbt
libraryDependencies += "com.github.morotsman" %% "cats-lote" % "0.1.0"
```

```scala
import cats.effect._
import com.github.morotsman.lote.api.builders._
import com.github.morotsman.lote.internal.interpreter.nconsole.JLineTerminal

object MyPresentation extends IOApp.Simple {

  val run: IO[Unit] =
    JLineTerminal.resource[IO]().use { implicit terminal =>
      val session = SessionBuilder[IO]()
        .addSlide(
          TextSlideBuilder()
            .withTitle("Hello")
            .withContent("Welcome to my presentation!")
            .build
        )
        .build

      session.run
    }
}
```

### Browser Client (Scala.js App)

```scala
// build.sbt
enablePlugins(ScalaJSPlugin)
scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }
libraryDependencies += "com.github.morotsman" %%% "cats-lote" % "0.1.0"
```

```scala
import cats.effect._
import com.github.morotsman.lote.api.builders._
import com.github.morotsman.lote.internal.interpreter.nconsole.XtermTerminal
import org.scalajs.dom

object MyBrowserPresentation extends IOApp.Simple {

  val run: IO[Unit] = {
    val container = dom.document.getElementById("terminal")
      .asInstanceOf[dom.HTMLElement]

    XtermTerminal[IO](container).flatMap { implicit terminal =>
      val session = SessionBuilder[IO]()
        .addSlide(
          TextSlideBuilder()
            .withTitle("Hello from the Browser!")
            .withContent("Same API, different backend.")
            .build
        )
        .build

      session.run
    }
  }
}
```

#### Companion HTML

```html
<!DOCTYPE html>
<html>
<head>
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/xterm/css/xterm.css" />
</head>
<body>
  <div id="terminal" style="width: 100%; height: 100vh;"></div>
  <script type="module" src="./target/scala-2.13/my-browser-presentation-opt/main.js"></script>
</body>
</html>
```

---

## Testing Locally (Without Publishing)

You don't need to publish the library to test the browser port. Just like the existing `examples` project uses `.dependsOn(lote)`, you can add a `browser-examples` subproject that depends on `loteJS` directly.

### build.sbt additions

```scala
lazy val browserExamples = (project in file("browser-examples"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(loteJS % "test->test;compile->compile")
  .settings(commonSettings)
  .settings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    scalaJSUseMainModuleInitializer := true,
    testFrameworks += new TestFramework("munit.Framework"),
  )

lazy val root = (project in file("."))
  .aggregate(loteJVM, loteJS, examples, browserExamples)
  .settings(name := "cats-lote")
```

### Project layout

```
browser-examples/
  src/main/scala/
    com/github/morotsman/examples/BrowserPresentationExample.scala
  src/main/resources/
    index.html
```

### Running it

```bash
# 1. Compile the Scala.js output
sbt browserExamples/fastLinkJS

# 2. The compiled JS lands in:
#    browser-examples/target/scala-2.13/browser-examples-fastopt/main.js

# 3. Serve with any static file server, e.g.:
npx http-server browser-examples/src/main/resources -o

# 4. Or use vite/webpack if you want hot-reload and npm dependency bundling
```

For xterm.js, you have two options during local development:

1. **CDN** — just reference it from `index.html` (simplest, no npm needed):
   ```html
   <script src="https://cdn.jsdelivr.net/npm/xterm/lib/xterm.js"></script>
   <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/xterm/css/xterm.css" />
   ```

2. **npm + bundler** — use `npm init` in `browser-examples/`, `npm install xterm`, and bundle with vite/webpack. More setup, but better for a real development workflow.

### Example app for local testing

```scala
package com.github.morotsman.examples

import cats.effect._
import com.github.morotsman.lote.api.builders._
import com.github.morotsman.lote.internal.interpreter.nconsole.XtermTerminal
import org.scalajs.dom

object BrowserPresentationExample extends IOApp.Simple {

  val run: IO[Unit] = {
    val container = dom.document.getElementById("terminal")
      .asInstanceOf[dom.HTMLElement]

    XtermTerminal[IO](container).flatMap { implicit terminal =>
      val session = SessionBuilder[IO]()
        .addSlide(
          TextSlideBuilder()
            .withTitle("Browser Test")
            .withContent("It works! Same API, running in a browser.")
            .build
        )
        .build

      session.run
    }
  }
}
```

This is the same workflow as your existing `examples` project — everything resolves locally, no publishing required.

---

## Migration Steps

1. **Add Scala.js plugins** to `project/plugins.sbt`
2. **Convert `lote` to a cross-project** — move shared code to `shared/`, JLine code to `jvm/`
3. **Replace `%%` with `%%%`** for cross-published dependencies (cats-core, cats-effect, munit)
4. **Create xterm.js facade** in `js/src/main/scala/`
5. **Implement `XtermTerminal`** using the facade + Cats Effect `Queue` + `Dispatcher`
6. **Verify shared code compiles on JS** — fix any JVM-only imports (there shouldn't be many since the core is pure FP)
7. **Create a minimal browser example** to validate end-to-end
8. **Publish** with `sbt loteJVM/publish loteJS/publish` (both artifacts get published)

## Future Browser Rendering Backends

The `Terminal[F[_]]` trait is backend-agnostic, so beyond xterm.js we could have additional browser implementations with richer rendering:

### Canvas2D Terminal

A `CanvasTerminal[F]` that renders text directly onto an HTML `<canvas>` element.

- **write()** — parses ANSI sequences and draws styled text using `CanvasRenderingContext2D.fillText()`
- **Pros**: Full control over fonts, anti-aliasing, custom glyph rendering, pixel-level effects (glow, blur, retro CRT look)
- **Cons**: Must implement our own ANSI parser and text layout (cursor tracking, scrolling, line wrapping)
- **Good for**: Stylized presentations, custom visual themes that go beyond what a terminal emulator offers

### WebGL / Three.js Terminal

A `ThreeJsTerminal[F]` that renders the terminal as a 3D scene using Three.js.

- **write()** — maps characters to textured quads or SDF text meshes in a Three.js scene
- **Pros**: Hardware-accelerated, 3D transitions (slides rotating in 3D space, camera fly-throughs), post-processing effects (bloom, depth of field, glitch)
- **Cons**: Most complex to implement, need to handle text rendering in WebGL (SDF fonts or texture atlases), heavier runtime
- **Good for**: Visually spectacular presentations, demo/conference keynotes

### Implementation Approach

All three backends (xterm.js, Canvas2D, Three.js) implement the same 5 methods:

```
Terminal[F[_]]
├── XtermTerminal    — ANSI passthrough to xterm.js (simplest, phase 1)
├── CanvasTerminal   — Custom 2D rendering on <canvas> (phase 2)
└── ThreeJsTerminal  — 3D scene rendering via Three.js (phase 3)
```

The client just picks which one to use:

```scala
// Swap one line to change rendering backend
XtermTerminal[IO](container)     // terminal emulator
CanvasTerminal[IO](canvas)       // 2D canvas
ThreeJsTerminal[IO](canvas)      // 3D WebGL
```

All presentation logic (slides, overlays, transitions, input handling) stays identical.

### Note on ANSI Parsing

xterm.js handles ANSI parsing for us, but Canvas2D and Three.js backends would need a shared ANSI sequence parser to extract styling (colors, bold, cursor movement) from the raw strings passed to `write()`. This parser should live in `shared/` so both backends can reuse it.

---

## Open Questions

- Should the library ship xterm.js as a bundled dependency, or require the user to include it via npm/CDN?
- Do we need a `Resource`-based constructor for `XtermTerminal` (like `JLineTerminal.resource`)? Probably yes, for clean lifecycle management.
- Mouse event mapping: xterm.js has its own mouse event API — need to map to the same internal representation used by JLine mouse tracking.
- Performance: for complex transitions/animations, verify that xterm.js render speed is acceptable (it should be — it's GPU-accelerated).

