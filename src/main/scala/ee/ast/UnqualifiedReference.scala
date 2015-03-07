package ee.ast

import ee.ast.types.TypeApplication

case class UnqualifiedReference(
  id             : Id,
  typeParameters : Seq[TypeApplication]
)
