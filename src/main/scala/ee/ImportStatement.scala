package ee

object ImportStatement extends Description(
  Annotation.* ~ `import` ~ ImportExpression ~ (`,` ~ ImportExpression).*
)

object ImportExpression extends Description(
  Reference ~ (`.` ~ (`_` | ImportSelectors)).?
)

object ImportSelectors extends Description(
  `{` ~ ImportSelector ~ (`,` ~ ImportSelector).* ~ `}`
)

object ImportSelector extends Description(
  (Reference | `_`) ~ (`=>` ~ (Reference | `_`)).?
)