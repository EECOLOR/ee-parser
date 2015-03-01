package ee

object Member extends Description(
  Annotation.* ~ Modifier.* ~ (Val | Var | Def | Type) ~ Assignment.?
)

object Val extends Description(
  `val` ~ Ids ~ (`,` ~ Ids).*
)
object Var extends Description(
  `var` ~ Ids ~ (`,` ~ Ids).*
)

object Ids extends Description(
  (Id ~ AssignedType.?) | Pattern
)

object Def extends Description(
  `def` ~ Id ~ TypeParameters.* ~ Parameters.* ~ AssignedType.? 
)

object Type extends Description(
  `type` ~ Id ~ TypeParameters.* ~ TypeConstraint.*
)
