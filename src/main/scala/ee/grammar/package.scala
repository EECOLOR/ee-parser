package ee

package object grammar {

  type Production[A <: Element] = (A, Element)

  type Start   = Nonterminal.TopLevelStatements.type
  type Grammar = (Nonterminal, Terminal, Production[_], Start)

  val  | = Choice
  type | = Choice
}
