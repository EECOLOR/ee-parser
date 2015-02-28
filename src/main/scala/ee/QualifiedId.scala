package ee

object QualifiedId extends Description(
  Id ~ (`.` ~ Id).*
)