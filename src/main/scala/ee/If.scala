package ee

object If extends Description(
  `if` ~ `(` ~ Expression ~ `)` ~ Expression ~ Else.?
)

object Else extends Description(
  `else` ~ Expression
)