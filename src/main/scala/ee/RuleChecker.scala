package ee

import org.qirx.programbuilder._
import scala.language.higherKinds

case class RuleProblem(rule: Rule, description: String)

object RuleChecker extends WithDefaultImplementation[DefinitionTraversal.Return :+: Static :+: Set :+: CNil] {

  import DefinitionTraversal.{ExtractRulesFromDefinition, ExtractChoicesFromDefinition}

  case class Check(rules: Map[Rule, Definition]) extends Return[Set[RuleProblem]](
    for {
      presenceProblems <- DetectPresenceProblems(rules)
      choiceProblems   <- DetectChoiceProblems(rules)
    } yield presenceProblems ++ choiceProblems
  )

  case class DetectPresenceProblems(rules: Map[Rule, Definition]) extends Return[Set[RuleProblem]](
    for {
      available         <- ValueOf(rules.keySet)
      definition        <- rules.values.toSet.toProgram
      inDefinition      <- ExtractRulesFromDefinition(definition)
      presenceProblems  <- CheckRulePresence(available, inDefinition)
    } yield presenceProblems
  )

  private val or = |

  case class DetectChoiceProblems(rules: Map[Rule, Definition]) extends Return[Set[RuleProblem]](
    for {
      definition     <- rules.values.toSet.toProgram
      choices        <- ExtractChoicesFromDefinition(definition)
      left or right  <- choices.toProgram
      choiceProblems <- DetectShadowingInChoices(left, right, rules)
    } yield choiceProblems
  )

  case class CheckRulePresence(available: Set[Rule], found: Set[Rule]) extends Return[Set[RuleProblem]](???)

  case class DetectShadowingInChoices(left: Definition, right: Definition, rules: Map[Rule, Definition])
    extends Return[Set[RuleProblem]](???)
}


