package ee.ast
package types

import ee.ast.meta.Decorations

case class TypeParameter(
  decorations    : Decorations,
  variance       : TypeParameter.Variance,
  name           : Id,
  typeParameters : Option[TypeParameterList],
  constraints    : Seq[TypeConstraint]
)

object TypeParameter {
  sealed trait Variance
  case object Invariant     extends Variance
  case object Covariant     extends Variance
  case object Contravariant extends Variance
}
