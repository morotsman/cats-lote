package com.github.morotsman.examples.slides

sealed trait Direction
case class DirectionUp() extends Direction
case class DirectionDown() extends Direction
case class DirectionRight() extends Direction
case class DirectionLeft() extends Direction
case class NoDirection() extends Direction

