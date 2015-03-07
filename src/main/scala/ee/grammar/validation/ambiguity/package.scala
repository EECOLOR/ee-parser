package ee.grammar
package validation

package object ambiguity {
  type Choices[A] = Set[A]
  type Sequence[A] = Seq[A]

  type Path = Seq[`Nonterminal | Terminal`]
}
