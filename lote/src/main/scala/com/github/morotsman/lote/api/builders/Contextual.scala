package com.github.morotsman.lote.api.builders

import cats.{Functor, Monad}

final case class Contextual[F[_], A] private[builders] (
    private[builders] val run: SlideContext[F] => A
) {
  def map[B](f: A => B): Contextual[F, B] =
    Contextual(ctx => f(run(ctx)))
}

object Contextual {
  def apply[F[_], A](f: SlideContext[F] => A): Contextual[F, A] =
    new Contextual(f)
}

final case class ContextualF[F[_], A] private[builders] (
    private[builders] val run: SlideContext[F] => F[A]
) {
  def map[B](f: A => B)(implicit functor: Functor[F]): ContextualF[F, B] =
    ContextualF(ctx => functor.map(run(ctx))(f))

  def flatMap[B](f: A => ContextualF[F, B])(implicit monad: Monad[F]): ContextualF[F, B] =
    ContextualF(ctx => monad.flatMap(run(ctx))(value => f(value).run(ctx)))
}

object ContextualF {
  def apply[F[_], A](f: SlideContext[F] => F[A]): ContextualF[F, A] =
    new ContextualF(f)
}
