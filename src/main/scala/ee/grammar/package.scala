package ee

package object grammar {

  type Production = (Nonterminal, Element)

  val  | = Choice
  type | = Choice
  val  ~ = Sequence
  type ~ = Sequence
}
