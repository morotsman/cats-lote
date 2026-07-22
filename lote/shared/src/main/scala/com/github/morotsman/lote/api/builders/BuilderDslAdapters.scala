package com.github.morotsman.lote.api.builders

import cats.Functor
import cats.effect.{Ref, Temporal}
import com.github.morotsman.lote.api.{Alignment, SlidePosition, TransitionType}
import com.github.morotsman.lote.api.spi.{Slide, Transition}
import com.github.morotsman.lote.internal.builders.{
  SlideBuilder => InternalSlideBuilder,
  TextSlideBuilder => InternalTextSlideBuilder
}
import com.github.morotsman.lote.internal.builders.SlideBuilder.{
  BuildState => SlideBuildState,
  SlideAdded,
  WithoutSlide
}
import com.github.morotsman.lote.internal.builders.TextSlideBuilder.{
  BuildState => TextSlideBuildState,
  ContentAdded,
  WithoutContent
}
import com.github.morotsman.lote.internal.model.SlideSpecification

private[lote] object BuilderDslAdapters {

  private trait MetadataDslAdapter[F[_], Builder, Self] extends SlideMetadataDsl[F, Self] {
    protected def ctx: SlideContext[F]

    protected def builder: Builder

    protected def wrap(builder: Builder): Self

    protected def applyTransition(builder: Builder, transition: Transition[F]): Builder

    protected def applyMorph(builder: Builder)(implicit temporal: Temporal[F], refMake: Ref.Make[F]): Builder

    protected def applyReplace(builder: Builder, replace: Char)(implicit
        temporal: Temporal[F],
        refMake: Ref.Make[F]
    ): Builder

    protected def applyFalling(builder: Builder, gravity: Double, selectAccelerator: Double)(implicit
        temporal: Temporal[F],
        refMake: Ref.Make[F]
    ): Builder

    protected def applyGrab(builder: Builder, stepSize: Int)(implicit
        temporal: Temporal[F],
        refMake: Ref.Make[F]
    ): Builder

    protected def applySmoke(builder: Builder)(implicit temporal: Temporal[F], refMake: Ref.Make[F]): Builder

    protected def applyDissolve(builder: Builder)(implicit temporal: Temporal[F], refMake: Ref.Make[F]): Builder

    protected def applyTitle(builder: Builder, title: String): Builder

    protected def applyPosition(builder: Builder, position: SlidePosition): Builder

    protected def applyPositionMerge(builder: Builder, position: SlidePosition): Builder

    protected def applyOffset(builder: Builder, dx: Double, dy: Double, dz: Double): Builder

    protected def applyRotationOffset(builder: Builder, drx: Double, dry: Double, drz: Double): Builder

    protected def applyWithFallback(builder: Builder, fallback: Transition[F]): Builder

    protected def applyWithFallbackType(builder: Builder, fallback: TransitionType)(implicit
        temporal: Temporal[F],
        refMake: Ref.Make[F]
    ): Builder

    override final def slideContext: SlideContext[F] = ctx

    protected final def update(next: Builder => Builder): Self =
      wrap(next(builder))

    override final def at(x: Double, y: Double, z: Double): Self =
      update(b => applyPosition(b, SlidePosition(x, y, z)))

    override final def rotatedBy(rx: Double, ry: Double, rz: Double): Self =
      update(b => applyPositionMerge(b, SlidePosition(rotX = rx, rotY = ry, rotZ = rz)))

    override final def position(pos: SlidePosition): Self =
      update(b => applyPosition(b, pos))

    override final def transparentBackground(): Self =
      update(b => applyPositionMerge(b, SlidePosition(transparentBackground = true)))

    override final def right(d: Double): Self = update(b => applyOffset(b, d, 0, 0))
    override final def left(d: Double): Self = update(b => applyOffset(b, -d, 0, 0))
    override final def down(d: Double): Self = update(b => applyOffset(b, 0, d, 0))
    override final def up(d: Double): Self = update(b => applyOffset(b, 0, -d, 0))
    override final def forward(d: Double): Self = update(b => applyOffset(b, 0, 0, d))
    override final def back(d: Double): Self = update(b => applyOffset(b, 0, 0, -d))
    override final def offset(dx: Double, dy: Double, dz: Double): Self = update(b => applyOffset(b, dx, dy, dz))

    override final def rotateX(degrees: Double): Self = update(b => applyRotationOffset(b, degrees, 0, 0))
    override final def rotateY(degrees: Double): Self = update(b => applyRotationOffset(b, 0, degrees, 0))
    override final def rotateZ(degrees: Double): Self = update(b => applyRotationOffset(b, 0, 0, degrees))

    override final def transition(transition: Transition[F]): Self =
      update(b => applyTransition(b, transition))

    override final def transition(sessionTransition: Contextual[F, Transition[F]]): Self =
      transition(sessionTransition.run(ctx))

    override final def morphTransition()(implicit temporal: Temporal[F], refMake: Ref.Make[F]): Self =
      update(b => applyMorph(b))

    override final def replaceTransition(replace: Char)(implicit temporal: Temporal[F], refMake: Ref.Make[F]): Self =
      update(b => applyReplace(b, replace))

    override final def fallingCharactersTransition(gravity: Double, selectAccelerator: Double)(implicit
        temporal: Temporal[F],
        refMake: Ref.Make[F]
    ): Self =
      update(b => applyFalling(b, gravity, selectAccelerator))

    override final def grabTransition(stepSize: Int)(implicit temporal: Temporal[F], refMake: Ref.Make[F]): Self =
      update(b => applyGrab(b, stepSize))

    override final def smokeTransition()(implicit temporal: Temporal[F], refMake: Ref.Make[F]): Self =
      update(b => applySmoke(b))

    override final def dissolveTransition()(implicit temporal: Temporal[F], refMake: Ref.Make[F]): Self =
      update(b => applyDissolve(b))

    override final def title(title: String): Self =
      update(b => applyTitle(b, title))

    override final def withFallback(fallback: Transition[F]): Self =
      update(b => applyWithFallback(b, fallback))

    override final def withFallback(fallback: Contextual[F, Transition[F]]): Self =
      withFallback(fallback.run(ctx))

    override final def withFallback(fallback: TransitionType)(implicit
        temporal: Temporal[F],
        refMake: Ref.Make[F]
    ): Self =
      update(b => applyWithFallbackType(b, fallback))
  }

  private trait SlideValueDslAdapter[F[_]] {
    protected def ctx: SlideContext[F]

    def addSlide(slide: Slide[F]): SlideBuilderReady[F]

    final def addSlide(slide: Contextual[F, Slide[F]]): SlideBuilderReady[F] =
      addSlide(slide.run(ctx))

    final def addSlideF(slide: ContextualF[F, Slide[F]])(implicit functor: Functor[F]): F[SlideBuilderReady[F]] =
      functor.map(slide.run(ctx))(addSlide)
  }

  private trait SlideMetadataAdapter[F[_], State <: SlideBuildState, Self]
      extends MetadataDslAdapter[F, InternalSlideBuilder[F, State], Self] {

    override protected final def applyTransition(
        builder: InternalSlideBuilder[F, State],
        transition: Transition[F]
    ): InternalSlideBuilder[F, State] =
      builder.transition(transition)

    override protected final def applyMorph(builder: InternalSlideBuilder[F, State])(implicit
        temporal: Temporal[F],
        refMake: Ref.Make[F]
    ): InternalSlideBuilder[F, State] =
      builder.morphTransition()(temporal, refMake, ctx.console, ctx.ticker, ctx.animationSettings)

    override protected final def applyReplace(builder: InternalSlideBuilder[F, State], replace: Char)(implicit
        temporal: Temporal[F],
        refMake: Ref.Make[F]
    ): InternalSlideBuilder[F, State] =
      builder.replaceTransition(replace)(temporal, refMake, ctx.console, ctx.ticker, ctx.animationSettings)

    override protected final def applyFalling(
        builder: InternalSlideBuilder[F, State],
        gravity: Double,
        selectAccelerator: Double
    )(implicit temporal: Temporal[F], refMake: Ref.Make[F]): InternalSlideBuilder[F, State] =
      builder.fallingCharactersTransition(gravity, selectAccelerator)(
        temporal,
        refMake,
        ctx.console,
        ctx.ticker,
        ctx.animationSettings
      )

    override protected final def applyGrab(builder: InternalSlideBuilder[F, State], stepSize: Int)(implicit
        temporal: Temporal[F],
        refMake: Ref.Make[F]
    ): InternalSlideBuilder[F, State] =
      builder.grabTransition(stepSize)(temporal, refMake, ctx.console, ctx.ticker, ctx.animationSettings)

    override protected final def applySmoke(builder: InternalSlideBuilder[F, State])(implicit
        temporal: Temporal[F],
        refMake: Ref.Make[F]
    ): InternalSlideBuilder[F, State] =
      builder.smokeTransition()(temporal, refMake, ctx.console, ctx.ticker, ctx.animationSettings)

    override protected final def applyDissolve(builder: InternalSlideBuilder[F, State])(implicit
        temporal: Temporal[F],
        refMake: Ref.Make[F]
    ): InternalSlideBuilder[F, State] =
      builder.dissolveTransition()(temporal, refMake, ctx.console, ctx.ticker, ctx.animationSettings)

    override protected final def applyTitle(
        builder: InternalSlideBuilder[F, State],
        title: String
    ): InternalSlideBuilder[F, State] =
      builder.title(title)

    override protected final def applyPosition(
        builder: InternalSlideBuilder[F, State],
        position: SlidePosition
    ): InternalSlideBuilder[F, State] =
      builder.position(position)

    override protected final def applyPositionMerge(
        builder: InternalSlideBuilder[F, State],
        position: SlidePosition
    ): InternalSlideBuilder[F, State] =
      builder.positionMerge(position)

    override protected final def applyOffset(
        builder: InternalSlideBuilder[F, State],
        dx: Double, dy: Double, dz: Double
    ): InternalSlideBuilder[F, State] =
      builder.offset(dx, dy, dz)

    override protected final def applyRotationOffset(
        builder: InternalSlideBuilder[F, State],
        drx: Double, dry: Double, drz: Double
    ): InternalSlideBuilder[F, State] =
      builder.rotateX(drx).rotateY(dry).rotateZ(drz)

    override protected final def applyWithFallback(
        builder: InternalSlideBuilder[F, State],
        fallback: Transition[F]
    ): InternalSlideBuilder[F, State] =
      builder.withFallback(fallback)(ctx.console)

    override protected final def applyWithFallbackType(
        builder: InternalSlideBuilder[F, State],
        fallback: TransitionType
    )(implicit temporal: Temporal[F], refMake: Ref.Make[F]): InternalSlideBuilder[F, State] =
      builder.withFallback(fallback)(temporal, refMake, ctx.console, ctx.ticker, ctx.animationSettings)
  }

  private trait TextMetadataAdapter[F[_], State <: TextSlideBuildState, Self]
      extends MetadataDslAdapter[F, InternalTextSlideBuilder[F, State], Self] {

    override protected final def applyTransition(
        builder: InternalTextSlideBuilder[F, State],
        transition: Transition[F]
    ): InternalTextSlideBuilder[F, State] =
      builder.transition(transition)

    override protected final def applyMorph(builder: InternalTextSlideBuilder[F, State])(implicit
        temporal: Temporal[F],
        refMake: Ref.Make[F]
    ): InternalTextSlideBuilder[F, State] =
      builder.morphTransition()(temporal, refMake, ctx.console, ctx.ticker, ctx.animationSettings)

    override protected final def applyReplace(builder: InternalTextSlideBuilder[F, State], replace: Char)(implicit
        temporal: Temporal[F],
        refMake: Ref.Make[F]
    ): InternalTextSlideBuilder[F, State] =
      builder.replaceTransition(replace)(temporal, refMake, ctx.console, ctx.ticker, ctx.animationSettings)

    override protected final def applyFalling(
        builder: InternalTextSlideBuilder[F, State],
        gravity: Double,
        selectAccelerator: Double
    )(implicit temporal: Temporal[F], refMake: Ref.Make[F]): InternalTextSlideBuilder[F, State] =
      builder.fallingCharactersTransition(gravity, selectAccelerator)(
        temporal,
        refMake,
        ctx.console,
        ctx.ticker,
        ctx.animationSettings
      )

    override protected final def applyGrab(builder: InternalTextSlideBuilder[F, State], stepSize: Int)(implicit
        temporal: Temporal[F],
        refMake: Ref.Make[F]
    ): InternalTextSlideBuilder[F, State] =
      builder.grabTransition(stepSize)(temporal, refMake, ctx.console, ctx.ticker, ctx.animationSettings)

    override protected final def applySmoke(builder: InternalTextSlideBuilder[F, State])(implicit
        temporal: Temporal[F],
        refMake: Ref.Make[F]
    ): InternalTextSlideBuilder[F, State] =
      builder.smokeTransition()(temporal, refMake, ctx.console, ctx.ticker, ctx.animationSettings)

    override protected final def applyDissolve(builder: InternalTextSlideBuilder[F, State])(implicit
        temporal: Temporal[F],
        refMake: Ref.Make[F]
    ): InternalTextSlideBuilder[F, State] =
      builder.dissolveTransition()(temporal, refMake, ctx.console, ctx.ticker, ctx.animationSettings)

    override protected final def applyTitle(
        builder: InternalTextSlideBuilder[F, State],
        title: String
    ): InternalTextSlideBuilder[F, State] =
      builder.title(title)

    override protected final def applyPosition(
        builder: InternalTextSlideBuilder[F, State],
        position: SlidePosition
    ): InternalTextSlideBuilder[F, State] =
      builder.position(position)

    override protected final def applyPositionMerge(
        builder: InternalTextSlideBuilder[F, State],
        position: SlidePosition
    ): InternalTextSlideBuilder[F, State] =
      builder.positionMerge(position)

    override protected final def applyOffset(
        builder: InternalTextSlideBuilder[F, State],
        dx: Double, dy: Double, dz: Double
    ): InternalTextSlideBuilder[F, State] =
      builder.offset(dx, dy, dz)

    override protected final def applyRotationOffset(
        builder: InternalTextSlideBuilder[F, State],
        drx: Double, dry: Double, drz: Double
    ): InternalTextSlideBuilder[F, State] =
      builder.rotateX(drx).rotateY(dry).rotateZ(drz)

    override protected final def applyWithFallback(
        builder: InternalTextSlideBuilder[F, State],
        fallback: Transition[F]
    ): InternalTextSlideBuilder[F, State] =
      builder.withFallback(fallback)(ctx.console)

    override protected final def applyWithFallbackType(
        builder: InternalTextSlideBuilder[F, State],
        fallback: TransitionType
    )(implicit temporal: Temporal[F], refMake: Ref.Make[F]): InternalTextSlideBuilder[F, State] =
      builder.withFallback(fallback)(temporal, refMake, ctx.console, ctx.ticker, ctx.animationSettings)
  }

  def slideStart[F[_]](
      ctx: SlideContext[F],
      builder: InternalSlideBuilder[F, WithoutSlide]
  ): SlideBuilderStart[F] =
    new SlideBuilderStartImpl[F](ctx, builder)

  def textSlideStart[F[_]](
      ctx: SlideContext[F],
      builder: InternalTextSlideBuilder[F, WithoutContent]
  ): TextSlideBuilderStart[F] =
    new TextSlideBuilderStartImpl[F](ctx, builder)

  private final class SlideBuilderStartImpl[F[_]](
      protected val ctx: SlideContext[F],
      protected val builder: InternalSlideBuilder[F, WithoutSlide]
  ) extends SlideBuilderStart[F]
      with SlideMetadataAdapter[F, WithoutSlide, SlideBuilderStart[F]]
      with SlideValueDslAdapter[F] {

    override protected def wrap(builder: InternalSlideBuilder[F, WithoutSlide]): SlideBuilderStart[F] =
      new SlideBuilderStartImpl(ctx, builder)

    override def addSlide(slide: Slide[F]): SlideBuilderReady[F] =
      new SlideBuilderReadyImpl(ctx, builder.addSlide(slide))
  }

  private final class SlideBuilderReadyImpl[
      F[_],
      State <: SlideBuildState with SlideAdded
  ](
      protected val ctx: SlideContext[F],
      protected val builder: InternalSlideBuilder[F, State]
  ) extends SlideBuilderReady[F]
      with SlideMetadataAdapter[F, State, SlideBuilderReady[F]]
      with SlideValueDslAdapter[F] {

    override protected def wrap(builder: InternalSlideBuilder[F, State]): SlideBuilderReady[F] =
      new SlideBuilderReadyImpl(ctx, builder)

    override def addSlide(slide: Slide[F]): SlideBuilderReady[F] =
      new SlideBuilderReadyImpl(ctx, builder.addSlide(slide))

    override def buildSpec(): SlideSpecification[F] =
      builder.build()
  }

  private final class TextSlideBuilderStartImpl[F[_]](
      protected val ctx: SlideContext[F],
      protected val builder: InternalTextSlideBuilder[F, WithoutContent]
  ) extends TextSlideBuilderStart[F]
      with TextMetadataAdapter[F, WithoutContent, TextSlideBuilderStart[F]] {

    override protected def wrap(builder: InternalTextSlideBuilder[F, WithoutContent]): TextSlideBuilderStart[F] =
      new TextSlideBuilderStartImpl(ctx, builder)

    override def content(content: String): TextSlideBuilderReady[F] =
      new TextSlideBuilderReadyImpl(ctx, builder.content(content))

    override def separator(separator: String): TextSlideBuilderStart[F] =
      new TextSlideBuilderStartImpl(ctx, builder.separator(separator))

    override def hint(hint: String): TextSlideBuilderStart[F] =
      new TextSlideBuilderStartImpl(ctx, builder.hint(hint))

    override def alignment(alignment: Alignment): TextSlideBuilderStart[F] =
      new TextSlideBuilderStartImpl(ctx, builder.alignment(alignment))
  }

  private final class TextSlideBuilderReadyImpl[
      F[_],
      State <: TextSlideBuildState with ContentAdded
  ](
      protected val ctx: SlideContext[F],
      protected val builder: InternalTextSlideBuilder[F, State]
  ) extends TextSlideBuilderReady[F]
      with TextMetadataAdapter[F, State, TextSlideBuilderReady[F]] {

    override protected def wrap(builder: InternalTextSlideBuilder[F, State]): TextSlideBuilderReady[F] =
      new TextSlideBuilderReadyImpl(ctx, builder)

    override def content(content: String): TextSlideBuilderReady[F] =
      new TextSlideBuilderReadyImpl(ctx, builder.content(content))

    override def step(step: String): TextSlideBuilderReady[F] =
      new TextSlideBuilderReadyImpl(ctx, builder.step(step))

    override def separator(separator: String): TextSlideBuilderReady[F] =
      new TextSlideBuilderReadyImpl(ctx, builder.separator(separator))

    override def hint(hint: String): TextSlideBuilderReady[F] =
      new TextSlideBuilderReadyImpl(ctx, builder.hint(hint))

    override def alignment(alignment: Alignment): TextSlideBuilderReady[F] =
      new TextSlideBuilderReadyImpl(ctx, builder.alignment(alignment))

    override def buildSpec(): SlideSpecification[F] =
      builder.build()
  }
}
