package ee

object Expression extends Description(
  ExpressionSuffix.?
)

object ExpressionSuffix extends Description(
  AssignedType | Assignment
)

