package ee

object TypeParameters extends Description(
  `[` ~ Type ~ (`,` ~ Type).* ~ `]`
)