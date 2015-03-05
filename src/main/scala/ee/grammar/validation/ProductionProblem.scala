package ee.grammar.validation

import ee.grammar.Nonterminal

sealed trait ProductionProblem {
  def description:String
}
case class NonterminalProblem(nonterminal: Nonterminal, description: String) extends ProductionProblem
