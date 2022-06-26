package com.github.morotsman
package lote.tools

import cats.implicits._
import cats.FlatMap

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
