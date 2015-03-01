package ee

object StringInterpolation extends Description(
  Reference ~ `"` ~ (Interpolation ~ !`"`) ~ `"`
)

object Interpolation extends Description(
  InterpolationEscape | `$` ~ Id | `$` ~ `{` ~ BlockStatements ~ `}`
)

object InterpolationEscape extends Description(
  `$` ~ `$`
)