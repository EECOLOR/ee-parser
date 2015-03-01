package ee

object Lambda extends Description(
  (Parameters | Parameter) ~ `=>` ~ Expression
)