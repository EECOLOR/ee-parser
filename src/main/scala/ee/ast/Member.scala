package ee.ast

import ee.ast.meta.Decorations
import ee.ast.types.TypeConstraint
import ee.ast.types.TypeParameterList
import ee.ast.types.`TypeReference | ExpandVariableArity`

sealed trait Member {
  def decorations : Decorations
  def name        : Id
  def body        : Option[`MacroReference | Expression`]
}

object Member {
  case class Type(
    decorations     : Decorations,
    name            : Id,
    typeParameters  : Seq[TypeParameterList],
    typeConstraints : Seq[TypeConstraint],
    body            : Option[`MacroReference | Expression`]
  ) extends Member

  case class Method(
    decorations    : Decorations,
    name           : Id,
    typeParameters : Seq[TypeParameterList],
    parameters     : Seq[ParameterList],
    returnType     : Option[`TypeReference | ExpandVariableArity`],
    body           : Option[`MacroReference | Expression`]
  ) extends Member

  case class Field(
    nature       : Field.Nature,
    decorations  : Decorations,
    name         : Id,
    assignedType : Option[`TypeReference | ExpandVariableArity`],
    body         : Option[`MacroReference | Expression`]
  ) extends Member

  object Field {
    sealed trait Nature
    case object Val         extends Nature
    case object Var         extends Nature
    case object Unspecified extends Nature
  }
}
