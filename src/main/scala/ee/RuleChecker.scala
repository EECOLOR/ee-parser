package ee

import org.qirx.programbuilder._
import scala.language.higherKinds
import scala.reflect.ClassTag

case class RuleProblem(rule: Rule, description: String)

object RuleChecker extends WithDefaultImplementation[Set :+: Static :+: CNil] {

  case class Check(rules: Map[Rule, Definition]) extends ReturnWithDefault[Set[RuleProblem]](
    for {
      presenceProblems <- detectPresenceProblems(rules)
      choiceProblems   <- detectChoiceProblems(rules)
    } yield presenceProblems ++ choiceProblems
  )

  case class CheckRulePresence(
    available: Set[Rule], 
    found: Set[Rule]
  ) extends Return[Set[RuleProblem]]

  case class DetectShadowingInChoices(
    left: Definition, 
    right: Definition, 
    rules: Map[Rule, Definition]
  ) extends Return[Set[RuleProblem]]

  private def detectPresenceProblems(rules: Map[Rule, Definition]) =
    for {
      available        <- ValueOf(rules.keySet)
      definition       <- rules.values.toSet.toProgram
      inDefinition     <- ValueOf(extractAll[Rule](definition))
      presenceProblems <- CheckRulePresence(available, inDefinition)
    } yield presenceProblems

  private def detectChoiceProblems(rules: Map[Rule, Definition]) =
    for {
      definition     <- rules.values.toSet.toProgram
      choices        <- ValueOf(extractAll[|](definition))
      left or right  <- choices.toProgram
      choiceProblems <- DetectShadowingInChoices(left, right, rules)
    } yield choiceProblems

  private def extractAll[T <: Definition: ClassTag](definition: Definition): Set[T] =
    (Set(definition) ++ traverseDefinitionsIn(definition)).flatMap {
      case found: T => Set(found)
      case _        => Set.empty[T]
    }

  private def traverseDefinitionsIn(definition: Definition): Set[Definition] =
    definition match {
      case c: CompoundDefinition => traverseDefinitionsIn(c.left) ++ traverseDefinitionsIn(c.right)
      case _                     => Set.empty[Definition]
    }

  private val or = |
}


