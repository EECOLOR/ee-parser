package ee

object Patterns extends Description(
  Pattern ~ (`|` ~ Pattern).*
)

object Pattern extends Description(
  TypedPattern | BoundPattern | OtherPattern
)

object TypedPattern extends Description(
  (Id | `_`) ~ AssignedType
)

object BoundPattern extends Description(
  Id ~ `@` ~ OtherPattern
)

object OtherPattern extends Description(
  InfixPattern | SimplePattern
)

object InfixPattern extends Description(
  SimplePattern ~ (Reference ~ SimplePattern).+
)

object SimplePattern extends Description(
  UnderscorePattern          |
  StringInterpolationPattern |
  Literal                    |
  ReferencePattern           |
  ProductPattern             |
  Id                         |
  SequencePattern
)

object UnderscorePattern extends Description(
  `_` ~ AssignedType.?
)

object StringInterpolationPattern extends Description(
  Reference ~ `"` ~ (InterpolationPattern ~ !`"`) ~ `"`
)

object InterpolationPattern extends Description(
  InterpolationEscape | `$` ~ Id | `$` ~ `{` ~ Pattern ~ `}`
)

object ReferencePattern extends Description(
  TypeReference ~ ProductPattern
)

object ProductPattern extends Description(
  `(` ~ Pattern ~ (`,` ~ Pattern).* ~`)`
)

object SequencePattern extends Description(
  `_` ~ `*`
)