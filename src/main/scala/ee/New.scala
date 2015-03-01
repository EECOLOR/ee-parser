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

object ParentTypes extends Description(
  ParentType ~ (`with` ~ ParentType).*
)

object ParentType extends Description(
  TypeReference ~ ProductExpression.*
)