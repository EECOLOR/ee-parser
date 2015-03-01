package ee

object TypeParameters extends Description(
  `[` ~ TypeParameter ~ (`,` ~ TypeParameter).* ~ `]`
)

object TypeParameter extends Description(
  Annotation.* ~ Variance.? ~ (Id | `_`) ~ TypeParameters ~ TypeConstraint.*
)

object Variance extends Description(
  `covariant` | `contravariant`
)
