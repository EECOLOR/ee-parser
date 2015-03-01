package ee

object Annotation extends Description(
  `@` ~ TypeReference ~ Application.*
)