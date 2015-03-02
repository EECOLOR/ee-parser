package ee

import org.qirx.programbuilder._
import scala.language.higherKinds

case class RuleProblem(rule: Rule, description: String)

object RuleChecker {

  def check[O[_]: ProgramType](rules: Map[Rule, Definition])(
    implicit e1: Set ~> O, e2: Static ~> O, e3: RuleChecker ~> O): Program[O]#Instance[RuleProblem] =

    for {
      withDefinition     <- ValueOf(rules.keySet)
      (rule, definition) <- rules.toSet.toProgram
      inDefinition       <- ExtractRulesFromDefinition(definition)
      presenceProblems   <- CheckRulePresence(withDefinition, inDefinition)
      problem            <- presenceProblems.toProgram
    } yield problem

  object DefaultRunner extends (RuleChecker ~> Id) {
    def transform[x] = _.defaultImplementation
  }
}

sealed trait RuleChecker[T] {
  def defaultImplementation: T
}

// Note to self: write tests before you implement these
case class ExtractRulesFromDefinition(definition: Definition) extends RuleChecker[Set[Rule]] {
  def defaultImplementation = ???
}

case class CheckRulePresence(withDefinition: Set[Rule], inDefinition: Set[Rule]) extends RuleChecker[Set[RuleProblem]] {
  def defaultImplementation = ???
}
