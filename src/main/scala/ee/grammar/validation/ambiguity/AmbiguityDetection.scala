package ee
package grammar
package validation
package ambiguity

import org.qirx.programbuilder._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.Deadline
import scala.util.Random

object AmbiguityDetection extends WithDefaultImplementation[Static :+: Set :+: CNil] {
/*
 * http://soft-dev.org/pubs/pdf/vasudevan_tratt__detecting_ambiguity_in_programming_language_grammars.pdf
 *
 *                   ___________               ________   Parsed     __________           ______
 *   Grammar        |           |  Sentence   |        |  output    /          \   Yes   |      |
 *   artefacts ---> | Sentence  | ----------> | Earley | --------> / Ambiguous? \ -----> | Stop |
 *                  | Generator |             | parser |           \            /        |______|
 *                  |___________|             |________|            \__________/             ^
 *                         ^  || uses                                     |                  |
 *                         |  ||                                       No |                  |
 *                         |  | ---> <Backend 1>                          |                  |
 *                         |   -------> <Backend 2>                       v                  |
 *                         |                                          __________             |
 *                         |                                     No  /          \  Yes       |
 *                          ----------------------------------------/   Time     \-----------
 *                                                                  \  exceeded? /
 *                                                                   \__________/
 */

  case object GetTerminalGenerator                    extends Return[TerminalGenerator]
  case object GetSeparatorGenerator                   extends Return[SeparatorGenerator]
  case class  CreateParser(grammarArtefacts: Grammar) extends Return[String => Boolean]

  case class Detect(
    grammar: Grammar,
    depth: Int,
    duration:FiniteDuration
  ) extends ReturnWithDefault[Option[AmbiguityProblem]](
      for {
        startTime <- GetTime
        backend   <- CreateBackend(grammar, depth)
        parser    <- CreateParser(grammar)
        result    <- Search(backend, parser, deadline = startTime + duration)
      } yield result
    )

  case class Search(
    backend: () => String,
    parser: String => Boolean,
    deadline: Deadline
  ) extends ReturnWithDefault[Option[AmbiguityProblem]](
    for {
      sentence  <- ValueOf(backend())
      ambiguous <- ValueOf(parser apply sentence)
      result    <- if (ambiguous) ambiguityProblem(sentence).toProgram
                   else if (deadline.isOverdue) ValueOf(None).toProgram
                   else Search(backend, parser, deadline).toProgram
    } yield result
  )

  case class CreateBackend(grammar:Grammar, threshold: Int) extends ReturnWithDefault[() => String](
    for {
      terminalGenerator  <- GetTerminalGenerator
      separatorGenerator <- GetSeparatorGenerator
      generator          <- GetSentenceGenerator(
                              grammar,
                              terminalGenerator,
                              separatorGenerator,
                              threshold
                            )
    } yield () => generator.generate(grammar.start)
  )

  case class GetSentenceGenerator(
    grammar: Grammar,
    terminalGenerator: TerminalGenerator,
    separatorGenerator: SeparatorGenerator,
    threshold: Int
  ) extends ReturnWithDefault[SentenceGenerator](
    Program(
      new DefaultSentenceGenerator(
        grammar,
        terminalGenerator,
        separatorGenerator,
        threshold
      )
    )
  )

  case object GetTime extends ReturnWithDefault[Deadline](ValueOf(Deadline.now))

  private[this] def ambiguityProblem(sentence: String) =
    ValueOf(Some(AmbiguityProblem(sentence, "The sentence led to an ambiguous result"))  )
}
