package ee.grammar

abstract class Productions[T](scope:T) extends Scope(scope) {

  private var _productions = Set.empty[Productions.Production]
  def productions = _productions

  implicit val addProduction:Productions.AddProduction =
    new Productions.AddProduction {
      def apply(nonterminal:Nonterminal, element: Element): Unit =
        _productions += nonterminal -> element
    }
}

object Productions {
  type Production = (Nonterminal, Element)

  abstract class AddProduction extends ((Nonterminal, Element) => Unit)
}
