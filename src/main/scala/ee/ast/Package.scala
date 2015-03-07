package ee.ast

import ee.ast.meta.Decorations

case class Package(
  name        : Path,
  decorations : Decorations,
  body        : Statements
)
