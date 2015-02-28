package ee

object ImportStatement extends Description(
  Annotation.* ~ `import` ~ ImportExpression ~ (`,` ~ ImportExpression).*
)

object ImportExpression extends Description(
  QualifiedId ~ (`.` ~ (`_` | ImportSelectors)).?
)

object ImportSelectors extends Description(
  `{` ~ ImportSelector ~ (`,` ~ ImportSelector).* ~ `}`
)

object ImportSelector extends Description(
  (QualifiedId | `_`) ~ (`=>` ~ (QualifiedId | `_`)).?
)