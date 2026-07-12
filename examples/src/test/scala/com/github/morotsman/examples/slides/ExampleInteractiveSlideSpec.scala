package com.github.morotsman.examples.slides

import cats.effect.{IO, Ref}
import cats.implicits._
import com.github.morotsman.lote.api.{AnimationSettings, Character, Key, Screen, SpecialKey}
import com.github.morotsman.lote.api.spi.{Ticker, TickerSubscription}
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

	private def positionsOf(frame: String): List[(Int, Int)] =
		frame
			.split("\n", -1)
			.toList
			.zipWithIndex
			.flatMap { case (line, row) =>
				line.zipWithIndex.collect {
					case ('W', col) => (row, col)
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
	  state <- Ref[IO].of(RecordingState())
	  slide = ExampleInteractiveSlide.fromAnimator[IO](recordingAnimator(state), console)
	  content <- slide.content
	} yield assertEquals(content.content, "    \n    \n    ")
  }

  test("startShow delegates to animator.animate") {
	for {
	  console <- TestNConsole.make()
	  state <- Ref[IO].of(RecordingState())
	  slide = ExampleInteractiveSlide.fromAnimator[IO](recordingAnimator(state), console)
	  _ <- slide.startShow
	  recorded <- state.get
	} yield assertEquals(recorded.animateCalls, 1)
  }

  test("stopShow delegates to animator.stop") {
	for {
	  console <- TestNConsole.make()
	  state <- Ref[IO].of(RecordingState())
	  slide = ExampleInteractiveSlide.fromAnimator[IO](recordingAnimator(state), console)
	  _ <- slide.stopShow
	  recorded <- state.get
	} yield assertEquals(recorded.stopCalls, 1)
  }

  test("userInput maps WASD keys to directions") {
	for {
	  console <- TestNConsole.make()
	  state <- Ref[IO].of(RecordingState())
	  slide = ExampleInteractiveSlide.fromAnimator[IO](recordingAnimator(state), console)
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
	  state <- Ref[IO].of(RecordingState())
	  slide = ExampleInteractiveSlide.fromAnimator[IO](recordingAnimator(state), console)
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
			ticker <- ManualTicker.make(step)
			animationSettings = AnimationSettings(step)
			_ = Random.setSeed(seed)
			animator <- Animator.create[IO](console, ticker, animationSettings)
			slide = ExampleInteractiveSlide.fromAnimator[IO](animator, console)
			_ <- slide.startShow
			_ <- ticker.tick(120)
			written <- console.writtenRef.get
			_ <- slide.stopShow
		} yield {
			val positions = written.reverse.map(positionsOf)
			val leftEdgeIndex = positions.indexWhere {
				case List((row, col)) => row == initialRow && col <= 1
				case _                => false
			}
			assert(
				leftEdgeIndex >= 0,
				s"Expected to observe 'W' at or immediately next to the left edge of row $initialRow, got: ${positions.take(80)}"
			)
			val framesAfterLeftEdge = positions.slice(leftEdgeIndex + 1, leftEdgeIndex + 11)
			assert(
				framesAfterLeftEdge.exists {
					case List((row, col)) => row == initialRow && col >= (width * 3) / 4
					case _                => false
				},
				s"Expected a frame after the left edge to keep 'W' on row $initialRow and place it back near the right edge, got: ${positions.slice(leftEdgeIndex, leftEdgeIndex + 10)}"
			)
		}
	}
}



