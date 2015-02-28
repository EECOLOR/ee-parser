package ee

object Parameters extends Description(
  `(` ~ Parameter ~ (`,` ~ Parameter).* ~ `)`
)

object Parameter extends Description(
  Modifier.* ~ Id ~ AssignedType.? ~ Assignment.?
)
