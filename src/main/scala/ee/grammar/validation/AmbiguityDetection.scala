package ee.grammar
package validation

import org.qirx.programbuilder._
import ee.WithDefaultImplementation
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.Deadline
import ee.grammar.{Choice => or}
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

  case class Detect(
    grammar: Grammar,
    depth: Int,
    duration:FiniteDuration
  ) extends ReturnWithDefault[Option[AmbiguityProblem]](
      for {
        startTime         <- GetTime
        backend           <- CreateBackend(grammar, depth)
        earleyParser      <- CreateEarleyParser(grammar)
        result            <- Search(backend, earleyParser, deadline = startTime + duration)
      } yield result
    )

  case class Search(
    backend: Backend,
    earleyParser: EarleyParser,
    deadline: Deadline
  ) extends ReturnWithDefault[Option[AmbiguityProblem]](
    for {
      sentence  <- ValueOf(backend())
      ambiguous <- ValueOf(earleyParser apply sentence)
      result    <- if (ambiguous) ValueOf(Some(AmbiguityProblem(sentence))).toProgram
                   else if (deadline.isOverdue) ValueOf(None).toProgram
                   else Search(backend, earleyParser, deadline).toProgram
    } yield result
  )

  case class CreateBackend(grammar:Grammar, threshold: Int) extends ReturnWithDefault[Backend](
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

  /* This is a mess. Probably best to move ambiguity stuff into
   * another package and create more files.
   */

  type Backend = () => String
  type EarleyParser = String => Boolean

  type Choices[A] = Set[A]
  type Sequence[A] = Seq[A]

  case class Grammar(start: Nonterminal, productions: Set[Production])

  case class Production(
    key: `Nonterminal | Terminal`,
    alternatives: Choices[Sequence[`Nonterminal | Terminal`]]
  )

  type Path = Seq[`Nonterminal | Terminal`]

  trait SeparatorGenerator {
    def generateFor(context: Path):String
  }

  trait TerminalGenerator {
    def generateFor(terminal: Terminal):String
  }

  trait SentenceGenerator {
    def generate(nonterminal: Nonterminal):String
  }

  case object GetTime extends ReturnWithDefault[Deadline](ValueOf(Deadline.now))

  case class CreateEarleyParser(grammarArtefacts: Grammar) extends Return[EarleyParser]

  case object GetTerminalGenerator extends Return[TerminalGenerator]
  case object GetSeparatorGenerator extends Return[SeparatorGenerator]

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

  class DefaultSentenceGenerator(
    grammar: Grammar,
    terminalGenerator: TerminalGenerator,
    separatorGenerator: SeparatorGenerator,
    threshold: Int
  ) extends SentenceGenerator {

    private val productionLookup = grammar.productions.flatMap(Production.unapply).toMap
    private val counters         = productionLookup.mapValues(_ => new Counters)

    def generate(nonterminal:Nonterminal):String =
      generate(nonterminal, depth = 0, path = Seq(nonterminal))

    private def generate(current: Nonterminal, depth: Int, path: Path):String =
      whileUpdatingCountersFor(current) {
        val alternative =
          if (depth >= threshold) selectScoredAlternative(current)
          else                    selectRandomAlternative(current)

        val separator = separatorGenerator generateFor path

        val sentence =
          alternative.foldLeft(Seq.empty[String]) {
            case (s, n: Nonterminal) => s :+ generate(n, depth + 1, path :+ n)
            case (s, t: Terminal)    => s :+ terminalGenerator.generateFor(t)
          }

        sentence mkString separator
      }

    private def selectRandomAlternative(current: Nonterminal) = {
      val alternatives = productionLookup(current)
      randomChoiceOf(alternatives)
    }

    private def selectScoredAlternative(current:Nonterminal) = {
      val scored = productionLookup(current) groupBy determineScore
      val (_, alternatives) = scored.minBy { case (score, _) => score }
      randomChoiceOf(alternatives)
    }

    private def randomChoiceOf(alternatives:Choices[Sequence[`Nonterminal | Terminal`]]) =
      alternatives.toVector(Random nextInt alternatives.size)

    private def determineScore(sequence: Sequence[`Nonterminal | Terminal`]):Double =
      sequence.foldLeft(0D) {
        case (score, n:Nonterminal) if (counters(n).entered > 0) =>
          val counter = counters(n)
          score + (1D - (counter.exited * (1D / counter.entered)))
        case (score, _) => score
      }

    private class Counters(
      var entered: Int = 0,
      var exited : Int = 0
    )

    private def whileUpdatingCountersFor[T](nonterminal:Nonterminal)(code: => T):T = {
      val counter = counters(nonterminal)
      counter.entered += 1
      val result = code
      counter.entered -= 1
      result
    }
  }
}
