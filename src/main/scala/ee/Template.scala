package ee

object Template extends Description(
  ???//TemplateSignatures ~ TemplateBody.?
)


object TemplateBody extends Description(
  `{` ~ SelfType.? ~ BlockStatements ~ `}`
)

object SelfType extends Description(
  (`_` | Id) ~ AssignedType.? ~ `=>`
)
