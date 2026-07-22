# Browser Examples

Browser-based examples for cats-lote, running via Scala.js and Three.js / WebGL.

## Available Examples

| Example | Main class | HTML | Description |
|---------|-----------|------|-------------|
| **SimpleAnimationExample** | `com.github.morotsman.examples.SimpleAnimationExample` | `simple.html` | Lightweight demo of the core animation building blocks (counter, glide, scroll, wipe transition). Fast startup, no 3D positioning. |
| **AdvancedWebGLExample** | `com.github.morotsman.examples.AdvancedWebGLExample` | `index.html` | The full `SharedAdvancedPresentation` with 3D spatial layout, camera navigation, and a 3D landscape with rolling hills, castles, and roaming figures. |

To switch between examples, change `Compile / mainClass` in `build.sbt`:

```scala
// For simple animation demos:
Compile / mainClass := Some("com.github.morotsman.examples.SimpleAnimationExample")

// For the full advanced presentation:
Compile / mainClass := Some("com.github.morotsman.examples.AdvancedWebGLExample")
```

## Quick Start

### 1. Compile the Scala.js output

From the project root, clean cross-project dependencies and compile:

```bash
sbt "sharedExamplesJS/clean; browserExamples/clean; browserExamples/fastLinkJS"
```

> **Why the clean?** The browser examples depend on `shared-examples`, where the presentation logic lives. sbt's incremental compiler sometimes doesn't notice changes in cross-project dependencies, so cleaning both projects ensures all changes are picked up.

### 2. Serve the files

**Important:** Start the server from the `browser-examples/` directory (not the project root):

```bash
cd browser-examples
python3 -m http.server 8080
```

### 3. Open in your browser

- **Simple animation demos:** [http://127.0.0.1:8080/simple.html](http://127.0.0.1:8080/simple.html)
- **Advanced WebGL presentation:** [http://127.0.0.1:8080/index.html](http://127.0.0.1:8080/index.html)

### All-in-one

From the project root, build and launch in one go:

```bash
sbt "sharedExamplesJS/clean; browserExamples/clean; browserExamples/fastLinkJS" && \
  cd browser-examples && \
  python3 -m http.server 8080 &
open http://127.0.0.1:8080/simple.html
```

## Development Workflow

For a fast edit-compile-refresh cycle:

```bash
# In one terminal — watch for changes and recompile automatically
sbt ~browserExamples/fastLinkJS

# In another terminal — serve the files
cd browser-examples
python3 -m http.server 8080
```

sbt will recompile whenever a source file changes. Just refresh the browser to pick up the new output.

## Stopping the Server

Press `Ctrl+C` in the terminal where `python3 -m http.server` is running to stop the file server.

If you are also running `sbt ~browserExamples/fastLinkJS` in watch mode, press `Ctrl+C` (or type `Enter` then `exit`) in that terminal to stop the sbt watch process.

## Project Structure

```
browser-examples/
├── index.html                          # Entry point for AdvancedWebGLExample
├── simple.html                         # Entry point for SimpleAnimationExample
├── src/main/scala/com/github/morotsman/examples/
│   ├── AdvancedWebGLExample.scala      # Full presentation with 3D landscape
│   ├── SimpleAnimationExample.scala    # Lightweight simple animation demos
│   └── landscape/
│       └── Landscape3DSlide.scala      # WebGL 3D landscape with rolling hills, castles, and roaming figures
└── target/scala-2.13/
    └── browserexamples-fastopt/
        └── main.js                     # Compiled Scala.js output (generated)
```

The presentation logic itself lives in `shared-examples/` (`SharedAdvancedPresentation`), which is shared with the terminal examples.

## Notes

- **Three.js** is loaded via CDN in `index.html` — no npm install needed.
- **Three.js version:** The Scala.js facades in `ThreeJsFacade.scala` target **Three.js r160** (`0.160.0`). The HTML files pin this exact version. If you upgrade Three.js, verify that the facades and rendering pipeline still work correctly. A runtime version check will log a console warning if the loaded Three.js revision doesn't match.
- The compiled JS uses **ES modules**, so you must serve over HTTP (opening `index.html` as a `file://` URL will not work).
- Use `fastLinkJS` during development (fast, larger output) and `fullLinkJS` for production (slower, optimized output).
