package ee

object Type extends Description(
  SingleType | VariableArityType
)

object SingleType extends Description(
  QualifiedId ~ TypeParameters.* ~ TypeProjection.*
)

object TypeProjection extends Description(
  `#` ~ SingleType
)

object VariableArityType extends Description (
  SingleType ~ `*`
)

object AssignedType extends Description(
  `:` ~ (Type | ExpandVariableArity)
)

object ExpandVariableArity extends Description(
  `_` ~ `*`
)