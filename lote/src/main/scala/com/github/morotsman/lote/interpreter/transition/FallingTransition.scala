package com.github.morotsman.lote.interpreter.transition

import cats.effect.kernel.Temporal
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide, Transition}
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted

import scala.concurrent.duration.DurationInt

object FallingTransition {
  def apply[F[_] : Temporal](): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): NConsole[F] => F[Unit] = console => {

      def getRows(from: ScreenAdjusted): List[String] = from.content.grouped(from.width).toList

      def falling(rowsToFall: Int, emptyRow: String, rows: List[String]): List[String] =
        List.fill(rowsToFall)(emptyRow) ::: rows.dropRight(rowsToFall)

      def fall(screenHeight: Int, screenWidth: Int, rowsToFall: Double, fallen: Int, emptyRow: String, rows: List[String]): F[Unit] = {
        if (fallen > screenHeight) {
          console.clear()
        } else {
          val newRows = falling(rowsToFall.toInt, emptyRow, rows)
          console.clear() >>
            console.writeString(ScreenAdjusted(newRows.mkString(""), screenWidth, screenHeight)) >>
            Temporal[F].sleep(40.milli) >>
            fall(screenHeight, screenWidth, rowsToFall * 1.2, fallen + rowsToFall.toInt, emptyRow, newRows)
        }
      }

      for {
        slide1 <- from.content(console)
        slide2 <- to.content(console)
        emptyRow = Array.fill(slide1.width)(" ").mkString("")
        rows = getRows(slide1)
        _ <- console.writeString(slide1) >> fall(slide1.height, slide1.width, 1, 0, emptyRow, rows) >> console.writeString(slide2)
      } yield ()

    }
  }


}
