package com.github.morotsman.lote.internal.interpreter.nconsole

/** Abstraction over a terminal, allowing injection for testing.
  */
private[lote] trait Terminal {
  def read(timeoutInMillis: Long): Int
  def width: Int
  def height: Int
  def flush(): Unit
  def write(s: String): Unit
  def close(): Unit = ()
}
