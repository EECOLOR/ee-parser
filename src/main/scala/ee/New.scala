package ee

object New extends Description(
  `new` ~ (ParentTypesWithPrefix | ParentTypesWithPrefix ~ TemplateBody | TemplateBody)
)

object ParentTypesWithPrefix extends Description(
  EarlyInitialization.? ~ ParentTypes
)

object EarlyInitialization extends Description(
  TemplateBody ~ `with`
)
