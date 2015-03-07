package ee.ast
package meta

sealed trait AccessModifier {
  def qualifiers: Seq[Reference]
}
object AccessModifier {
  final case class Public   (qualifiers: Seq[Reference]) extends AccessModifier
  final case class Private  (qualifiers: Seq[Reference]) extends AccessModifier
  final case class Protected(qualifiers: Seq[Reference]) extends AccessModifier

  object Public    { val empty = Public(Seq.empty)    }
  object Private   { val empty = Private(Seq.empty)   }
  object Protected { val empty = Protected(Seq.empty) }
}
