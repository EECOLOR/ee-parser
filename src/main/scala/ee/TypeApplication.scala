package ee

object TypeApplication extends Description(
  `[` ~ TypeReference ~ (`,` ~ TypeReference) ~ `]`
)