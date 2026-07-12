package com.github.morotsman.examples.slides

import cats.effect.{IO, Ref}
import cats.implicits._
import com.github.morotsman.lote.algebra.{AnimationSettings, NConsole, Ticker, TickerSubscription}
import com.github.morotsman.lote.model.{Character, Key, Screen, SpecialKey}
import com.github.morotsman.lote.support.TestNConsole
import munit.CatsEffectSuite

import scala.concurrent.duration._
import scala.util.Random

final case class RecordingState(
	animateCalls: Int = 0,
	stopCalls: Int = 0,
	directions: List[Direction] = Nil
)

final case class ManualTicker(
	subscribersRef: Ref[IO, Vector[(Long, IO[Unit])]],
	nextIdRef: Ref[IO, Long],
	step: FiniteDuration
) extends Ticker[IO] {

	override def subscribe(callback: IO[Unit]): IO[TickerSubscription[IO]] =
		for {
			id <- nextIdRef.getAndUpdate(_ + 1)
			_ <- subscribersRef.update(_ :+ (id -> callback))
		} yield new TickerSubscription[IO] {
			override def cancel: IO[Unit] =
				subscribersRef.update(_.filterNot(_._1 == id))
		}

	override def start: IO[Unit] = IO.unit

	override def stop: IO[Unit] =
		subscribersRef.set(Vector.empty)

	def tick(times: Int = 1): IO[Unit] =
		(0 until times).toList.traverse_ { _ =>
			IO.sleep(step) *> subscribersRef.get.flatMap(_.toList.traverse_(_._2))
		}
}

object ManualTicker {
	def make(step: FiniteDuration): IO[ManualTicker] =
		for {
			subscribersRef <- Ref[IO].of(Vector.empty[(Long, IO[Unit])])
			nextIdRef <- Ref[IO].of(0L)
		} yield ManualTicker(subscribersRef, nextIdRef, step)
}

class ExampleInteractiveSlideSpec extends CatsEffectSuite {

  private def recordingAnimator(state: Ref[IO, RecordingState]): Animator[IO] =
	new Animator[IO] {
	  override def animate(): IO[Unit] =
		state.update(s => s.copy(animateCalls = s.animateCalls + 1))

	  override def stop(): IO[Unit] =
		state.update(s => s.copy(stopCalls = s.stopCalls + 1))

	  override def changeDirection(input: Direction): IO[Unit] =
		state.update(s => s.copy(directions = s.directions :+ input))
	}

	private def positionsOf(frame: String, symbol: Char): List[(Int, Int)] =
		frame
			.split("\n", -1)
			.toList
			.zipWithIndex
			.flatMap { case (line, row) =>
				line.zipWithIndex.collect {
					case (c, col) if c == symbol => (row, col)
				}
			}

	private def heartIndexesForSeed(seed: Long, screenSize: Int): Set[Int] = {
		val random = new Random(seed)
		List.fill(500)(random.nextInt(screenSize)).toSet
	}

	private def seedWithoutHearts(screenSize: Int, blocked: Set[Int]): Long =
		Iterator
			.from(0)
			.map(_.toLong)
			.find(seed => heartIndexesForSeed(seed, screenSize).intersect(blocked).isEmpty)
			.getOrElse(throw new AssertionError("Could not find deterministic seed without hearts on the wrap path"))

  test("content returns an empty centered screen") {
	for {
	  console <- TestNConsole.make(screen = Screen(4, 3))
	  implicit0(nc: NConsole[IO]) = console: NConsole[IO]
	  state <- Ref[IO].of(RecordingState())
	  slide = ExampleInteractiveSlide.make[IO](recordingAnimator(state))
	  content <- slide.content
	} yield assertEquals(content.content, "    \n    \n    ")
  }

  test("startShow delegates to animator.animate") {
	for {
	  console <- TestNConsole.make()
	  implicit0(nc: NConsole[IO]) = console: NConsole[IO]
	  state <- Ref[IO].of(RecordingState())
	  slide = ExampleInteractiveSlide.make[IO](recordingAnimator(state))
	  _ <- slide.startShow
	  recorded <- state.get
	} yield assertEquals(recorded.animateCalls, 1)
  }

  test("stopShow delegates to animator.stop") {
	for {
	  console <- TestNConsole.make()
	  implicit0(nc: NConsole[IO]) = console: NConsole[IO]
	  state <- Ref[IO].of(RecordingState())
	  slide = ExampleInteractiveSlide.make[IO](recordingAnimator(state))
	  _ <- slide.stopShow
	  recorded <- state.get
	} yield assertEquals(recorded.stopCalls, 1)
  }

  test("userInput maps WASD keys to directions") {
	for {
	  console <- TestNConsole.make()
	  implicit0(nc: NConsole[IO]) = console: NConsole[IO]
	  state <- Ref[IO].of(RecordingState())
	  slide = ExampleInteractiveSlide.make[IO](recordingAnimator(state))
	  _ <- slide.userInput(Character('a'))
	  _ <- slide.userInput(Character('w'))
	  _ <- slide.userInput(Character('s'))
	  _ <- slide.userInput(Character('d'))
	  recorded <- state.get
	} yield assertEquals(
	  recorded.directions,
	  List(
		DirectionLeft(),
		DirectionUp(),
		DirectionDown(),
		DirectionRight()
	  )
	)
  }

  test("userInput ignores non-WASD input") {
	for {
	  console <- TestNConsole.make()
	  implicit0(nc: NConsole[IO]) = console: NConsole[IO]
	  state <- Ref[IO].of(RecordingState())
	  slide = ExampleInteractiveSlide.make[IO](recordingAnimator(state))
	  _ <- slide.userInput(Character('x'))
	  _ <- slide.userInput(Key(SpecialKey.Enter))
	  recorded <- state.get
	} yield assertEquals(recorded.directions, Nil)
  }

	test("the worm wraps from the left edge to the same row") {
		val width = 80
		val height = 300
		val step = 1.millis
		val screenSize = width * (height - 1)
		val initialHeadIndex = screenSize / 2
		val initialRow = initialHeadIndex / width
		val rowStart = initialHeadIndex - (initialHeadIndex % width)
		val rowEnd = rowStart + width - 1
		val wrapPath = (rowStart to rowEnd).toSet
		val seed = seedWithoutHearts(screenSize, wrapPath)

		for {
			console <- TestNConsole.make(screen = Screen(width, height))
			implicit0(nc: NConsole[IO]) = console: NConsole[IO]
			ticker <- ManualTicker.make(step)
			implicit0(tk: Ticker[IO]) = ticker: Ticker[IO]
			implicit0(animationSettings: AnimationSettings) = AnimationSettings(step)
			_ = Random.setSeed(seed)
			animator <- Animator.make[IO]()
			slide = ExampleInteractiveSlide.make[IO](animator)
			_ <- slide.startShow
			_ <- ticker.tick(120)
			written <- console.writtenRef.get
			_ <- slide.stopShow
		} yield {
			val positions = written.reverse.map(frame => positionsOf(frame, 'W'))
			val leftEdgeIndex = positions.indexOf(List((initialRow, 0)))
			assert(
				leftEdgeIndex >= 0,
				s"Expected to observe 'W' at the left edge of row $initialRow, got: ${positions.take(80)}"
			)
			val firstFrameAfterLeftEdge = positions.drop(leftEdgeIndex + 1).headOption
			assert(
				firstFrameAfterLeftEdge.exists {
					case List((row, col)) => row == initialRow && col > 0
					case _                => false
				},
				s"Expected the first observed frame after the left edge to keep 'W' on row $initialRow and place it back on the right side, got: ${positions.drop(leftEdgeIndex).take(10)}"
			)
		}
	}
}



