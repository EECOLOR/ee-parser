package ee.ast
package meta

sealed trait SpecialModifier
object SpecialModifier {
  case object Implicit        extends SpecialModifier

  case object GenerateEquals  extends SpecialModifier
  case object GenerateUnapply extends SpecialModifier
  case object GenerateApply   extends SpecialModifier

  case object Abstract        extends SpecialModifier
  case object Sealed          extends SpecialModifier
  case object Synchronized    extends SpecialModifier
  case object Volatile        extends SpecialModifier
  case object Transient       extends SpecialModifier
  case object Native          extends SpecialModifier
  case object Package         extends SpecialModifier
  case object Override        extends SpecialModifier
  case object Lazy            extends SpecialModifier
  case object Final           extends SpecialModifier
  case object Static          extends SpecialModifier
}
