package ee.grammar

abstract class Productions[T](scope:T) extends Scope(scope) {

  private[this] var _productions = Set.empty[Production]
  def productions: Set[Production] = _productions

  implicit val addProduction:Productions.AddProduction =
    new Productions.AddProduction {
      def apply(nonterminal:Nonterminal, element: Element): Unit =
        _productions += nonterminal -> element
    }
}

object Productions {
  abstract class AddProduction extends ((Nonterminal, Element) => Unit)
}
