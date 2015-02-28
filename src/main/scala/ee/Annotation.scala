package ee

object Annotation extends Description(
  `@` ~ QualifiedId ~ TypeParameters.* ~ Parameters.*
)