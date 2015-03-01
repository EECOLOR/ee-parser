package ee

object TypeConstraint extends Description(
  `<:` ~ TypeReference |
  `>:` ~ TypeReference |
  `<%` ~ TypeReference |
  `:`  ~ TypeReference
)