package ee.grammar
package validation.ambiguity

case class Production(
  key: `Nonterminal | Terminal`,
  alternatives: Choices[Sequence[`Nonterminal | Terminal`]]
)
