package ee.ast

final case class TopLevel(
  packages  : Seq[Package],
  imports   : Seq[Import],
  templates : Seq[Template]
)
object TopLevel {
  val empty = TopLevel(Seq.empty, Seq.empty, Seq.empty)
}
