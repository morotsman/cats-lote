package com.github.morotsman.lote.api

import munit.FunSuite

class WebGLConfigSpec extends FunSuite {

  // ── Default config is valid ──

  test("default config — creates successfully") {
    val config = WebGLConfig()
    assertEquals(config.nearClip, 1.0)
    assertEquals(config.farClip, 100000.0)
    assertEquals(config.fieldOfView, 50.0)
    assertEquals(config.antialias, true)
    assertEquals(config.backgroundColor, "#000000")
    assertEquals(config.devicePixelRatio, None)
    assertEquals(config.cameraTransitionBaseMs, 800.0)
    assertEquals(config.cameraTransitionFactor, 1.8)
    assertEquals(config.maxZoom, 20.0)
    assertEquals(config.maxFieldOfView, 160.0)
    assertEquals(config.cellWidth, 10)
    assertEquals(config.cellHeight, 20)
  }

  test("custom config — all fields can be overridden") {
    val config = WebGLConfig(
      nearClip = 0.5,
      farClip = 50000.0,
      fieldOfView = 60.0,
      antialias = false,
      backgroundColor = "#1a1a2e",
      devicePixelRatio = Some(2.0),
      cameraTransitionBaseMs = 500.0,
      cameraTransitionFactor = 2.0,
      maxZoom = 10.0,
      maxFieldOfView = 120.0,
      cellWidth = 8,
      cellHeight = 16
    )
    assertEquals(config.nearClip, 0.5)
    assertEquals(config.farClip, 50000.0)
    assertEquals(config.fieldOfView, 60.0)
    assertEquals(config.antialias, false)
    assertEquals(config.backgroundColor, "#1a1a2e")
    assertEquals(config.devicePixelRatio, Some(2.0))
    assertEquals(config.cellWidth, 8)
    assertEquals(config.cellHeight, 16)
  }

  // ── nearClip validation ──

  test("nearClip — must be positive") {
    intercept[IllegalArgumentException] { WebGLConfig(nearClip = 0.0) }
    intercept[IllegalArgumentException] { WebGLConfig(nearClip = -1.0) }
  }

  // ── farClip validation ──

  test("farClip — must be greater than nearClip") {
    intercept[IllegalArgumentException] { WebGLConfig(nearClip = 10.0, farClip = 10.0) }
    intercept[IllegalArgumentException] { WebGLConfig(nearClip = 10.0, farClip = 5.0) }
  }

  test("farClip — just above nearClip is valid") {
    val config = WebGLConfig(nearClip = 10.0, farClip = 10.001)
    assertEquals(config.farClip, 10.001)
  }

  // ── fieldOfView validation ──

  test("fieldOfView — must be between 0 and 180 exclusive") {
    intercept[IllegalArgumentException] { WebGLConfig(fieldOfView = 0.0) }
    intercept[IllegalArgumentException] { WebGLConfig(fieldOfView = 180.0) }
    intercept[IllegalArgumentException] { WebGLConfig(fieldOfView = -10.0) }
    intercept[IllegalArgumentException] { WebGLConfig(fieldOfView = 200.0) }
  }

  test("fieldOfView — boundary values just inside range are valid") {
    val low = WebGLConfig(fieldOfView = 0.1)
    assertEquals(low.fieldOfView, 0.1)
    val high = WebGLConfig(fieldOfView = 179.9)
    assertEquals(high.fieldOfView, 179.9)
  }

  // ── maxZoom validation ──

  test("maxZoom — must be positive") {
    intercept[IllegalArgumentException] { WebGLConfig(maxZoom = 0.0) }
    intercept[IllegalArgumentException] { WebGLConfig(maxZoom = -1.0) }
  }

  // ── maxFieldOfView validation ──

  test("maxFieldOfView — must be between 0 (exclusive) and 180 (inclusive)") {
    intercept[IllegalArgumentException] { WebGLConfig(maxFieldOfView = 0.0) }
    intercept[IllegalArgumentException] { WebGLConfig(maxFieldOfView = -10.0) }
    intercept[IllegalArgumentException] { WebGLConfig(maxFieldOfView = 181.0) }
  }

  test("maxFieldOfView — 180 is valid (inclusive upper bound)") {
    val config = WebGLConfig(maxFieldOfView = 180.0)
    assertEquals(config.maxFieldOfView, 180.0)
  }

  // ── cellWidth / cellHeight validation ──

  test("cellWidth — must be positive") {
    intercept[IllegalArgumentException] { WebGLConfig(cellWidth = 0) }
    intercept[IllegalArgumentException] { WebGLConfig(cellWidth = -1) }
  }

  test("cellHeight — must be positive") {
    intercept[IllegalArgumentException] { WebGLConfig(cellHeight = 0) }
    intercept[IllegalArgumentException] { WebGLConfig(cellHeight = -1) }
  }

  // ── cameraTransition validation ──

  test("cameraTransitionBaseMs — must be non-negative") {
    intercept[IllegalArgumentException] { WebGLConfig(cameraTransitionBaseMs = -1.0) }
  }

  test("cameraTransitionBaseMs — zero is valid") {
    val config = WebGLConfig(cameraTransitionBaseMs = 0.0)
    assertEquals(config.cameraTransitionBaseMs, 0.0)
  }

  test("cameraTransitionFactor — must be non-negative") {
    intercept[IllegalArgumentException] { WebGLConfig(cameraTransitionFactor = -0.1) }
  }

  test("cameraTransitionFactor — zero is valid") {
    val config = WebGLConfig(cameraTransitionFactor = 0.0)
    assertEquals(config.cameraTransitionFactor, 0.0)
  }
}
