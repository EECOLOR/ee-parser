package ee.grammar
package validation

import org.qirx.programbuilder._
import ee.WithDefaultImplementation
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.Deadline

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

  type ???
  type Backend = ???
  type Backends = Set[Backend]
  type Productions = Set[Production[_]]
  type EarleyParser = String => Boolean
  type SentenceGenerator = Set[String]

  case class Detect(
    grammarArtefacts: Productions,
    backends: Backends, duration:FiniteDuration
  ) extends ReturnWithDefault[Set[???]](
      for {
        start             <- GetTime
        sentenceGenerator <- CreateSentenceGenerator(grammarArtefacts, backends)
        earleyParser      <- CreateEarleyParser(grammarArtefacts)
        result            <- Search(sentenceGenerator, earleyParser, deadline = start + duration)
      } yield result
    )

  case class Search(
    sentenceGenerator:SentenceGenerator,
    earleyParser:EarleyParser,
    deadline: Deadline
  ) extends ReturnWithDefault[Set[???]](
    for {
      sentence  <- sentenceGenerator.toProgram
      ambiguous <- ValueOf(earleyParser apply sentence)
      result    <- if (ambiguous) noResult
                   else if (deadline.isOverdue) noResult
                   else Search(sentenceGenerator, earleyParser, deadline).toProgram
    } yield result
  )

  def noResult = ValueOf(Set.empty[???]).toProgram

  case class CreateSentenceGenerator(grammarArtefacts: Productions, backends: Backends)
    extends Return[SentenceGenerator]

  case class CreateEarleyParser(grammarArtefacts: Productions)
    extends Return[EarleyParser]

  case object GetTime extends ReturnWithDefault[Deadline](ValueOf(Deadline.now))
}
