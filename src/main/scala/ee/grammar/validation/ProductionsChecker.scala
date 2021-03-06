package ee
package grammar
package validation

import grammar.{Choice => or}

import org.qirx.programbuilder._

import scala.reflect.ClassTag

object ProductionsChecker extends WithDefaultImplementation[Set :+: Static :+: CNil] {

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

  private[this] def detectPresenceProblems(productions: Productions) =
    for {
      available        <- ValueOf(productions.keySet)
      definition       <- productions.values.toSet.toProgram
      inDefinition     <- ValueOf(extractAll[Nonterminal](definition))
      presenceProblems <- CheckNonterminalPresence(available, inDefinition)
    } yield presenceProblems

  private[this] def detectChoiceProblems(productions: Productions) =
    for {
      definition     <- productions.values.toSet.toProgram
      choices        <- ValueOf(extractAll[Choice](definition))
      left or right  <- choices.toProgram
      choiceProblems <- DetectShadowingInChoices(left, right, productions)
    } yield choiceProblems

  private[this] def detectAmbiguityProblems(productions: Productions) =
    ValueOf(???).toProgram

  private[this] def extractAll[T <: Element : ClassTag](definition: Element): Set[T] =
    (Set(definition) ++ traverseDefinitionsIn(definition)).flatMap {
      case found: T => Set(found)
      case _        => Set.empty[T]
    }

  private[this] def traverseDefinitionsIn(definition: Element): Set[Element] =
    definition match {
      case CompoundElement(left, right) => traverseDefinitionsIn(left) ++ traverseDefinitionsIn(right)
      case _                  => Set.empty
    }

  private[this] object CompoundElement {
    def unapply(e:Element):Option[(Element, Element)] =
      Option(e).flatMap {
        case Sequence(head, tail)  => Some( (head, tail) )
        case Choice  (left, right) => Some( (left, right) )
        case _                     => None
      }
  }
}


