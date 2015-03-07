package ee.ast.meta

final case class Decorations(
  annotations      : Seq[Annotation],
  accessModifiers  : Seq[AccessModifier],
  specialModifiers : Seq[SpecialModifier]
)
object Decorations {
  val empty = Decorations(Seq.empty, Seq.empty, Seq.empty)
}
