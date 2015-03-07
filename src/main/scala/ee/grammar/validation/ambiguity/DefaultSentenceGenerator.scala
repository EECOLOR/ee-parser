package ee.grammar
package validation.ambiguity

import scala.util.Random

class DefaultSentenceGenerator(
  grammar: Grammar,
  terminalGenerator: TerminalGenerator,
  separatorGenerator: SeparatorGenerator,
  threshold: Int
) extends SentenceGenerator {

  private val productionLookup = grammar.productions.flatMap(Production.unapply).toMap
  private val counters         = productionLookup.mapValues(_ => new Counters)

  def generate(nonterminal: Nonterminal):String =
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
