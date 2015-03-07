package ee.ast
package types

sealed trait `TypeReference | ExpandVariableArity`

sealed trait TypeReference extends `TypeReference | ExpandVariableArity`
object TypeReference {

  case class Single(
    reference    : Reference,
    projections  : Seq[Single],
    existentials : Structural
  ) extends TypeReference

  case class Structural(
    body: Template.Body
  ) extends TypeReference

}

// Union types would allow us to move this to another file
case object ExpandVariableArity extends `TypeReference | ExpandVariableArity`
