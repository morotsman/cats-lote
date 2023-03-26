package kubernetes.session1

import cats.effect.{IO, IOApp}
import com.github.morotsman.lote.builders.PresentationBuilder
import com.github.morotsman.lote.interpreter.PresentationExecutorInterpreter
import com.github.morotsman.lote.interpreter.middleware.{Middleware, Timer}
import com.github.morotsman.lote.interpreter.nconsole.NConsole
import com.github.morotsman.lote.interpreter.transition.{FallingCharactersTransition, FallingTransition, MorphTransition, ReplaceTransition}
import com.github.morotsman.lote.model.{Alignment, HorizontalAlignment, VerticalAlignment}
import kubernetes.session1.slides.{Agenda, Bye, Start}


object Session1 extends IOApp.Simple {

  private val instruction1 =
    """
      |Install kubectl: https://kubernetes.io/docs/tasks/tools/
      |""".stripMargin

  private val instruction2 =
    """
      |Install kubectl: https://kubernetes.io/docs/tasks/tools/
      |Install kubectx (makes it easy to switch between contexts): https://github.com/ahmetb/kubectx
      |""".stripMargin

  private val instruction3 =
    """
      |Install kubectl: https://kubernetes.io/docs/tasks/tools/
      |Install kubectx (makes it easy to switch between contexts): https://github.com/ahmetb/kubectx
      |Create a ~/.kube folder if you don't already have it
      |""".stripMargin

  private val instruction4 =
    """
      |Install kubectl: https://kubernetes.io/docs/tasks/tools/
      |Install kubectx (makes it easy to switch between contexts): https://github.com/ahmetb/kubectx
      |Create a ~/.kube folder if you don't already have it
      |Copy the ConfigurationManagement/kubectl/config file to ~/.kube
      |""".stripMargin

  private val instruction5 =
    """
      |Install kubectl: https://kubernetes.io/docs/tasks/tools/
      |Install kubectx (makes it easy to switch between contexts): https://github.com/ahmetb/kubectx
      |Create a ~/.kube folder if you don't already have it
      |Copy the ConfigurationManagement/kubectl/config file to ~/.kube
      |Copy the ConfigurationManagement/kubectl/verisure-kubeauth file to ~/.kube
      |""".stripMargin

  private val instruction6 =
    """
      |Install kubectl: https://kubernetes.io/docs/tasks/tools/
      |Install kubectx (makes it easy to switch between contexts): https://github.com/ahmetb/kubectx
      |Create a ~/.kube folder if you don't already have it
      |Copy the ConfigurationManagement/kubectl/config file to ~/.kube
      |Copy the ConfigurationManagement/kubectl/verisure-kubeauth file to ~/.kube
      |
      |Test by:
      |kubectl get services
      |""".stripMargin

  override def run(): IO[Unit] = {
    val presentation = PresentationBuilder[IO]()
      .addTextSlide {
        _.content(Start())
          .transition(out = ReplaceTransition(' '))
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
      }
      .addTextSlide {
        _.content(Agenda())
          .transition(out = FallingCharactersTransition(1.2, 1.1))
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
      }
      .addTextSlide {
        _.content(instruction1)
          .transition(out = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .addTextSlide {
        _.content(instruction2)
          .transition(out = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .addTextSlide {
        _.content(instruction3)
          .transition(out = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .addTextSlide {
        _.content(instruction4)
          .transition(out = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .addTextSlide {
        _.content(instruction5)
          .transition(out = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .addTextSlide {
        _.content(instruction6)
          .transition(out = MorphTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Left))
      }
      .addTextSlide {
        _.content(Bye())
          .transition(out = FallingCharactersTransition())
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
      }
      .addTextSlide {
        _.content("")
          .alignment(Alignment(VerticalAlignment.Up, HorizontalAlignment.Center))
      }
      .build()

    for {
      console <- NConsole.make[IO]()
      middleware <- Middleware.make[IO](console)
      executor <- PresentationExecutorInterpreter.make[IO](middleware, presentation)
      _ <- executor.start()
    } yield ()
  }

}
