package ee

object Template extends Description(
  Decorations ~ TemplateType ~ Id ~ TypeParameters.* ~ Constructor ~ ParentTypes ~ TemplateBody.?
)

object TemplateType extends Description(
  `trait` | `object` | `class`
)

object Constructor extends Description(
  Decorations ~ Parameters.*
)

object TemplateBody extends Description(
  `{` ~ SelfType.? ~ BlockStatements ~ `}`
)

object SelfType extends Description(
  (`_` | Id) ~ AssignedType.? ~ `=>`
)
