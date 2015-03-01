package ee

object TypeParameters extends Description(
  `[` ~ TypeParameter ~ (`,` ~ TypeParameter).* ~ `]`
)

object TypeParameter extends Description(
  Decorations ~ Variance.? ~ (Id | `_`) ~ TypeParameters ~ TypeConstraint.*
)

object Variance extends Description(
  `covariant` | `contravariant`
)
