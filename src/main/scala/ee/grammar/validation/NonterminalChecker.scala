package ee.grammar.validation

import scala.reflect.ClassTag

import org.qirx.programbuilder._

import ee.WithDefaultImplementation
import ee.grammar.{Choice => or}
import ee.grammar.Choice
import ee.grammar.CompoundElement
import ee.grammar.Element
import ee.grammar.Nonterminal

object NonterminalChecker extends WithDefaultImplementation[Set :+: Static :+: CNil] {

  // http://soft-dev.org/pubs/pdf/vasudevan_tratt__detecting_ambiguity_in_programming_language_grammars.pdf

  type Productions = Map[Nonterminal, Element]

  case class Check(productions: Productions) extends ReturnWithDefault[Set[NonterminalProblem]](
    for {
      presenceProblems  <- detectPresenceProblems(productions)
      choiceProblems    <- detectChoiceProblems(productions)
      ambiguityProblems <- detectAmbiguityProblems(productions)
    } yield presenceProblems ++ choiceProblems
  )

  case class CheckNonterminalPresence(
    available: Set[Nonterminal],
    found:     Set[Nonterminal]
  ) extends Return[Set[NonterminalProblem]]

  case class DetectShadowingInChoices(
    left:        Element,
    right:       Element,
    productions: Productions
  ) extends Return[Set[NonterminalProblem]]

  private def detectPresenceProblems(productions: Productions) =
    for {
      available        <- ValueOf(productions.keySet)
      definition       <- productions.values.toSet.toProgram
      inDefinition     <- ValueOf(extractAll[Nonterminal](definition))
      presenceProblems <- CheckNonterminalPresence(available, inDefinition)
    } yield presenceProblems

  private def detectChoiceProblems(productions: Productions) =
    for {
      definition     <- productions.values.toSet.toProgram
      choices        <- ValueOf(extractAll[Choice](definition))
      left or right  <- choices.toProgram
      choiceProblems <- DetectShadowingInChoices(left, right, productions)
    } yield choiceProblems

  private def detectAmbiguityProblems(productions: Productions) =
    for {
      _ <- ValueOf(???)
    } yield ???

  private def extractAll[T <: Element : ClassTag](definition: Element): Set[T] =
    (Set(definition) ++ traverseDefinitionsIn(definition)).flatMap {
      case found: T => Set(found)
      case _        => Set.empty[T]
    }

  private def traverseDefinitionsIn(definition: Element): Set[Element] =
    definition match {
      case c: CompoundElement => traverseDefinitionsIn(c.left) ++ traverseDefinitionsIn(c.right)
      case _                  => Set.empty
    }
}


