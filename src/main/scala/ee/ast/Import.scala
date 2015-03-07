package ee.ast

import ee.ast.meta.Decorations

sealed trait Import {
  def decorations: Decorations
}

object Import {
  case class Single(
    decorations : Decorations,
    reference   : Reference
  ) extends Import

  case class All(
    decorations : Decorations,
    reference   : Reference
  ) extends Import

  case class Renamed(
    decorations : Decorations,
    reference   : Reference,
    newName     : Path
  ) extends Import
}
