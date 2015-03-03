package ee

import org.qirx.programbuilder._
import scala.reflect.ClassTag

object DefinitionTraversal extends WithDefaultImplementation[Set :+: Static :+: CNil] {
  case class ExtractRulesFromDefinition(definition: Definition) extends Return[Set[Rule]](
    Extract(definition)(implicitly[ClassTag[Rule]]).toProgram
  )
  case class ExtractChoicesFromDefinition(definition: Definition) extends Return[Set[|]](
    Extract(definition)(implicitly[ClassTag[|]]).toProgram
  )

  case class Extract[T <: Definition: ClassTag, __](definition: Definition) extends Return[Set[T]](
    for {
      definitions <- TraverseDefinitionsIn(definition)
      found <- ExtractFromSet(definitions)
    } yield found
  )

  case class ExtractFromSet[T <: Definition: ClassTag, __](definitions: Set[Definition]) extends Return[Set[T]](
    ValueOf {
      definitions.flatMap {
        case found: T => Set(found)
        case _        => Set.empty[T]
      }
    }
  )

  val or = |

  case class TraverseDefinitionsIn(definition: Definition) extends Return[Set[Definition]](
    definition match {
      case compound: CompoundDefinition =>
        for {
          definitionsL <- TraverseDefinitionsIn(compound.left)
          definitionsR <- TraverseDefinitionsIn(compound.right)
        } yield Set(compound) ++ definitionsL ++ definitionsR
      case definition =>
        ValueOf(Set(definition))
    }
  )
}