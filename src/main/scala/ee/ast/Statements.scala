package ee.ast

final case class Statements(
  imports     : Seq[Import],
  templates   : Seq[Template],
  members     : Seq[Member],
  expressions : Seq[Expression]
)
object Statements {
  val empty = Statements(Seq.empty, Seq.empty, Seq.empty, Seq.empty)
}
