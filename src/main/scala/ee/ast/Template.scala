package ee.ast

import ee.ast.meta.Decorations
import ee.ast.types.`TypeReference | ExpandVariableArity`
import ee.ast.types.TypeParameterList

case class Template(
  decorations    : Decorations,
  nature         : Template.Nature,
  name           : Id,
  typeParameters : Seq[TypeParameterList],
  constructor    : Option[Template.Constructor],
  initialBody    : Template.Body,
  parents        : Seq[Template.Parent],
  body           : Template.Body
)

object Template {
  sealed trait Nature
  case object Trait  extends Nature
  case object Class  extends Nature
  case object Object extends Nature
  case object Enum   extends Nature

  case class Constructor(
    decorations : Decorations,
    parameters  : NonEmptySeq[ParameterList]
  )

  case class Parent(
    reference : Reference,
    arguments : Seq[ArgumentList]
  )

  final case class Body(
    self : Option[Id],
    assignedType : Option[`TypeReference | ExpandVariableArity`],
    body : Statements
  )
  object Body {
    val empty = Body(None, None, Statements.empty)
  }
}
