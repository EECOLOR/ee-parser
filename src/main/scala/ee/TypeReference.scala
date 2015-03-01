package ee

object TypeReference extends Description(
  SingleTypeRefence | StructuralType | VariableArityType
)

object SingleTypeRefence extends Description(
  Reference ~ TypeProjection.*
)

object TypeProjection extends Description(
  `#` ~ SingleTypeRefence
)

object VariableArityType extends Description (
  SingleTypeRefence ~ `*`
)

object StructuralType extends Description(
  TemplateBody
)

object AssignedType extends Description(
  `:` ~ (TypeReference | ExpandVariableArity)
)

object ExpandVariableArity extends Description(
  `_` ~ `*`
)

