package kubernetes.session1.slides

import cats.Monad
import cats.effect.Temporal
import cats.implicits._
import com.github.morotsman.lote.algebra.{NConsole, Slide}
import com.github.morotsman.lote.interpreter.nconsole.NConsole.ScreenAdjusted
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, UserInput, VerticalAlignment}

import scala.concurrent.duration.DurationInt
import scala.util.Random

object Bye {
  def apply(): String =
    """
      |
      |
      |
      |
      |
      |
      |
      |
      |___________.__                                .___
      |\__    ___/|  |__   ____     ____   ____    __| _/
      |  |    |   |  |  \_/ __ \  _/ __ \ /    \  / __ |
      |  |    |   |   Y  \  ___/  \  ___/|   |  \/ /_/ |
      |  |____|   |___|  /\___  >  \___  >___|  /\____ |
      |                \/     \/       \/     \/      \/
      |
      |""".stripMargin

}
