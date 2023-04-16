package com.github.morotsman
package lote.tools

import cats.FlatMap
import cats.implicits._

object DebugHelper {
  implicit class DebugHelper[F[_] : FlatMap, A](fa: F[A]) {

    def debug: F[A] =
      for {
        a <- fa
        tn = Thread.currentThread.getName
        _ = println(s"[$tn}] $a")
      } yield a
  }
}
