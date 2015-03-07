package ee

import ee.ast.types.TypeParameter
package object ast {
  type NonEmptySeq[+A] = Seq[A]

  type ParameterList = Seq[Parameter]
  type Parameter     = Member

  type ArgumentList = Seq[Expression]

  // Think about elements having a (global) path or location or address
}
