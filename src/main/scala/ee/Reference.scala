package ee

object Reference extends Description(
  UnqualifiedReference ~ (`.` ~ UnqualifiedReference).*
)

object UnqualifiedReference extends Description(
  Id ~ TypeApplication.*
)