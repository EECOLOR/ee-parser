package ee.ast.types

sealed trait TypeConstraint {
  def reference: TypeReference
}
object TypeConstraint {
  case class IsSubTypeOf  (reference: TypeReference) extends TypeConstraint
  case class IsSuperTypeOf(reference: TypeReference) extends TypeConstraint
  case class CanBeSeenAs  (reference: TypeReference) extends TypeConstraint
  case class HasTypeClass (reference: TypeReference) extends TypeConstraint
}
