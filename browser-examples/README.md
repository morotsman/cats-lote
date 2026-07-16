# Browser Examples

Browser-based examples for cats-lote, running via Scala.js and xterm.js.

## Available Examples

| Example | Main class | Description |
|---------|-----------|-------------|
| **AdvancedExample** | `com.github.morotsman.examples.AdvancedExample` | Full 21-slide presentation showcasing text slides, staged reveals, overlays, transitions, custom slides, and the interactive worm game. |
| **BrowserPresentationExample** | `com.github.morotsman.examples.BrowserPresentationExample` | Minimal 3-slide example — useful as a starting point. |

## Quick Start

### 1. Compile the Scala.js output

From the project root:

```bash
sbt browserExamples/fastLinkJS
```

### 2. Serve the files

**Important:** Start the server from the `browser-examples/` directory (not the project root):

```bash
cd browser-examples
python3 -m http.server 8080
```

Then open [http://127.0.0.1:8080/](http://127.0.0.1:8080/) in your browser.

> If you accidentally start the server from the project root you'll see a directory listing instead of the presentation.

## Choosing Which Example to Run

The default main class is `AdvancedExample`. To switch to a different example, set the main class in `build.sbt`:

```scala
Compile / mainClass := Some("com.github.morotsman.examples.BrowserPresentationExample")
```

Alternatively, you can override it from the sbt shell without editing `build.sbt`:

```bash
sbt 'set browserExamples / Compile / mainClass := Some("com.github.morotsman.examples.BrowserPresentationExample")' browserExamples/fastLinkJS
```

After changing the main class, re-run `fastLinkJS` and refresh the browser.

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
├── index.html                          # Entry point served to the browser
├── src/main/scala/com/github/morotsman/examples/
│   ├── AdvancedExample.scala           # Full-featured presentation
│   ├── BrowserPresentationExample.scala # Minimal example
│   └── slides/
│       ├── Bye.scala                   # ASCII art goodbye screen
│       ├── Direction.scala             # Direction types for the game
│       ├── ExampleInteractiveSlide.scala # Interactive worm/snake game
│       └── SweepRightTransition.scala  # Custom sweep-right transition
└── target/scala-2.13/
    └── browserexamples-fastopt/
        └── main.js                     # Compiled Scala.js output (generated)
```

## Notes

- **xterm.js** is loaded via CDN in `index.html` — no npm install needed.
- The compiled JS uses **ES modules**, so you must serve over HTTP (opening `index.html` as a `file://` URL will not work).
- Use `fastLinkJS` during development (fast, larger output) and `fullLinkJS` for production (slower, optimized output).


