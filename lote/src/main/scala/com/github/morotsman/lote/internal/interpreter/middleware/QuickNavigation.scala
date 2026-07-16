package com.github.morotsman.lote.internal.interpreter.middleware

import cats.effect.Ref
import cats.{Applicative, Monad}
import cats.implicits._
import com.github.morotsman.lote.api.{Character, Key, Screen, ScreenAdjusted, SpecialKey, UserInput}
import com.github.morotsman.lote.api.spi.Overlay
import com.github.morotsman.lote.internal.util.Colors

private[lote] case class NavigationSubscriber[F[_]](id: Long, callback: Int => F[Unit])

private[lote] trait NavigationSubscription[F[_]] {
  def cancel: F[Unit]
}

private case class QuickNavigationState[F[_]](
    showQuickNavigation: Boolean = false,
    subscribers: List[NavigationSubscriber[F]] = List.empty[NavigationSubscriber[F]],
    titles: Vector[String] = Vector(),
    currentIndex: Int = 0,
    longestTitle: Int = 0
)

private[lote] trait QuickNavigation[F[_]] extends Overlay[F] {
  def setTitles(titles: Vector[String]): F[Unit]
  def subscribe(subscriber: NavigationSubscriber[F]): F[NavigationSubscription[F]]
  def onSlideChange(currentIndex: Int): F[Unit]
}

private[lote] object QuickNavigation {
  def make[F[_]: Monad: Ref.Make](): F[QuickNavigation[F]] = {
    val startRow = 0

    Ref[F].of(QuickNavigationState[F]()).map { quickNavigationState =>
      new QuickNavigation[F] {

        private def formatNavLink(title: String, length: Int, selected: Boolean): String = {
          if (selected) {
            Colors.bright + "> " + title + " " * (length - title.length) + Colors.reset
          } else {
            Colors.gray + "  " + title + " " * (length - title.length) + Colors.reset
          }
        }

        override def applyOverlay(
            context: Screen,
            screenAdjusted: ScreenAdjusted,
            originalContent: ScreenAdjusted
        ): F[ScreenAdjusted] = quickNavigationState.get.map { currentState =>
          {
            if (currentState.showQuickNavigation) {
              val tmp = screenAdjusted.content
                .split("\n")
                .zipWithIndex
                .map { case (line, index) =>
                  if (index >= startRow) {
                    currentState.titles
                      .get(index - startRow)
                      .map(t =>
                        formatNavLink(t, currentState.longestTitle, currentState.currentIndex == index - startRow)
                      )
                      .getOrElse(" " * (currentState.longestTitle + 2)) + " | " + line
                      .dropRight(currentState.longestTitle + 5)
                  } else {
                    " " * (currentState.longestTitle + 5) + line.dropRight(currentState.longestTitle + 5)
                  }
                }
                .mkString("\n")
              screenAdjusted.copy(content = tmp)
            } else {
              screenAdjusted
            }
          }
        }

        override def onUserInput(userInput: UserInput)(implicit F: Applicative[F]): F[Unit] =
          for {
            currentState <- quickNavigationState.get
            _ <- userInput match {
              case Character('N') =>
                quickNavigationState.set(
                  currentState.copy(
                    showQuickNavigation = !currentState.showQuickNavigation,
                    subscribers = currentState.subscribers
                  )
                )
              case Key(SpecialKey.Down) if currentState.currentIndex < currentState.titles.length - 1 =>
                quickNavigationState.set(
                  currentState
                    .copy(currentIndex = currentState.currentIndex + 1, subscribers = currentState.subscribers)
                )
              case Key(SpecialKey.Up) if currentState.currentIndex > 0 =>
                quickNavigationState.set(
                  currentState
                    .copy(currentIndex = currentState.currentIndex - 1, subscribers = currentState.subscribers)
                )
              case Key(SpecialKey.Enter) if currentState.showQuickNavigation =>
                currentState.subscribers.traverse_(_.callback(currentState.currentIndex))
              case _ => Monad[F].unit
            }
          } yield ()

        override def setTitles(titles: Vector[String]): F[Unit] =
          for {
            currentState <- quickNavigationState.get
            _ <- quickNavigationState.set(
              currentState.copy(
                titles = titles,
                longestTitle = if (titles.isEmpty) 0 else titles.map(_.length).max,
                subscribers = currentState.subscribers
              )
            )
          } yield ()

        override def subscribe(subscriber: NavigationSubscriber[F]): F[NavigationSubscription[F]] =
          for {
            currentState <- quickNavigationState.get
            _ <- quickNavigationState.set(
              currentState.copy(
                subscribers = subscriber :: currentState.subscribers
              )
            )
          } yield new NavigationSubscription[F] {
            override def cancel: F[Unit] =
              quickNavigationState.update(s => s.copy(subscribers = s.subscribers.filterNot(_.id == subscriber.id)))
          }

        override def onSlideChange(currentIndex: Int): F[Unit] =
          for {
            currentState <- quickNavigationState.get
            _ <-
              if (currentState.currentIndex != currentIndex) {
                quickNavigationState.set(
                  currentState.copy(
                    currentIndex = currentIndex
                  )
                )
              } else Monad[F].unit
          } yield ()
      }
    }
  }
}
