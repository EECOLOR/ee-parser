package ee.grammar

object DefaultRules extends Productions(Nonterminal) {
  import scope._, Metadata.Decorations, Element.Operations, Terminal._

  TopLevelStatements := (Package | Import | Template).*
  BlockStatements    := (          Import | Template | Member | Expression).*

  Package := Decorations ~ `package` ~ Value.QualifiedId ~ Expression.Block.?

  new Scope(Import) { import scope._
    Import             := Decorations ~ `import` ~ Expression ~ (`,` ~ Expression).*
      Expression       := Single | All | Selectors
      Single           := Value.Reference
      All              := Value.Reference ~ `.` ~ `__`
      Selectors        := `{` ~ Selector ~ (`,` ~ Selector).* ~ `}`
      Selector         := SimpleSelector | RenamingSelector
      SimpleSelector   := Value.Reference | `__`
      RenamingSelector := SimpleSelector ~ `=>` ~ SimpleSelector
  }

  new Scope(Template) { import scope._
    Template      := Decorations ~ Nature ~ Id ~ Type.Parameters.* ~ Constructor.? ~ Extends.? ~ Body.?
      Nature      := `trait` | `object` | `class`
      Constructor := Decorations ~ Value.Parameters.+
      Extends     := `extends` ~ Parents
    Parents       := InitialBody.? ~ Parent ~ (`with` ~ Parent).*
      InitialBody := Body ~ `with`
      Parent      := Value.Reference ~ Value.Application.*
    Body          := `{` ~ Self.? ~ BlockStatements ~ `}`
      Self        := (`__` | Id) ~ Type.Assignment.? ~ `=>`
    New           := `new` ~ (Parents | Parents ~ Body | Body)
  }

  new Scope(Member) { import scope._
    Member    := Decorations ~ (Type | Def | Val | Var | Default) ~ Value.Assignment.?
      Type    :=  `type` ~ Id ~ Type.Parameters.* ~ Type.Constraint.*
      Def     := `def` ~ Id ~ Type.Parameters.* ~ Value.Parameters.* ~ Type.Assignment.?
      Val     := `val` ~ Ids ~ (`,` ~ Ids).*
      Var     := `var` ~ Ids ~ (`,` ~ Ids).*
      Ids     := Default | Pattern
      Default := Id ~ Type.Assignment.?
  }

  new Scope(Type) { import scope._
    Assignment            := `:` ~ (Reference | ExpandVariableArity)
    Parameters            := `[` ~ Parameter ~ (`,` ~ Parameter).* ~ `]`
      Parameter           := Decorations ~ Variance.? ~ Id ~ Parameters.? ~ Constraint.*
      Variance            := `covariant` | `contravariant`
      Constraint          := (`<:` | `>:` | `<%` | `:`) ~ Reference
    Application           := `[` ~ Reference ~ (`,` ~ Reference) ~ `]`
    Reference             := Infix | Single | Structural | VariableArity | `__`
      Infix               := Reference ~ Reference ~ Reference
      Single              := Value.Reference ~ Projection.*
      Structural          := Template.Body
      VariableArity       := Single ~ `*`
      Projection          := `#` ~ Single
      ExpandVariableArity := `__` ~ `*`
  }

  new Scope(Value) { import scope._
    Assignment             := `=` ~ (`macro` ~ Reference | Expression)
    Parameters             := `(` ~ Parameter ~ (`,` ~ Parameter).* ~ `)`
      Parameter            := Member
    Reference              := UnqualifiedReference ~ (`.` ~ UnqualifiedReference).*
      UnqualifiedReference := Id ~ Type.Application.*
    QualifiedId            := Id ~ (`.` ~ Id).*
    Application            := Expression.Group
  }

  new Scope(Metadata) { import scope._
    Decorations       := Annotation.* ~ Modifier.*
      Annotation      := `@` ~ Value.Reference ~ Value.Application.*
      Modifier        := AccessModifier | SpecialModifier
      AccessModifier  := (`public` | `private` | `protected`) ~ AccessQualifier.?
      AccessQualifier := `[` ~ Value.Reference ~ (`,` ~ Value.Reference).* ~ `]`
      SpecialModifier :=  `implicit` | `override` | `abstract` | `package` | `case`
  }

  new Scope(Expression) { import scope._, Value.UnqualifiedReference
    Expression         := KeywordTriggered | Lambda | Compound

      KeywordTriggered := If | Do  | While | Try | Throw | Return | For | TickEmbedded | Template.New
        If             := `if` ~ `(` ~ Expression ~ `)` ~ Expression ~ (`else` ~ Expression).?
        Do             := `do` ~ Expression ~ `while` ~ `(` ~ Expression ~ `)`
        While          := `while` ~ `(` ~ Expression ~ `)` ~ Expression
        Try            := `try` ~ Expression ~ (Catch ~ Finally.? | Finally)
        Catch          := `catch` ~ Expression
        Finally        := `finally` ~ Expression
        Throw          := `throw` ~ Expression
        Return         := `return` ~ Expression.?
        For            := `for` ~ `{` ~ Generator ~ (Generator | Member | Guard).* ~ `}` ~ `yield`.? ~ Expression
        Generator      := Id ~ `<-` ~ Expression ~ Guard.?
        Guard          := `if` ~ Expression
        TickEmbedded   := `'''` ~ Id ~ !`'''` ~ `'''` // '''

      Lambda           := ((Value.Parameters | Value.Parameter) ~ `=>`).+ ~ Expression

      Compound         := (Infix | Prefix | Simple) ~ Postfix.? ~ Suffix.?
        Prefix         := PrefixOperator ~ Simple
        Infix          := (Prefix | Simple) ~ (UnqualifiedReference ~ (Prefix | Simple)).+
        Postfix        := UnqualifiedReference
        Suffix         := Match | Type.Assignment | Value.Assignment
        Match          :=  `match` ~ Cases
        Cases          := `{` ~ Case.+ ~ `}`
        Case           := `case` ~ Pattern ~ (`|` ~ Pattern).* ~ Guard.? ~ `=>` ~ BlockStatements

      Simple                  := (Group | Primitive) ~ (`.` ~ UnqualifiedReference | Application).* ~ `__`.?
        Group                 := Product | Cases | Block
          Product             := `(` ~ Expression ~ (`,` ~ Expression).* ~ `)`
          Block               := `{` ~ BlockStatements ~ `}`
        Primitive             := Literal | StringInterpolation | Value.Reference | `__`
          StringInterpolation := Value.Reference ~ `"` ~ (Interpolation | !`"`).+ ~ `"` // "
          Interpolation       := InterpolationEscape | `S` ~ Id | `S` ~ Block
          InterpolationEscape := `S` ~ `S`
  }

  new Scope(Pattern) { import scope._
    Pattern                := Typed | Bound | Infix | Simple
      Typed                := (Id | `__`) ~ Type.Assignment
      Bound                :=  Id ~ `@` ~ (Infix | Simple)
      Infix                := Simple ~ (Value.Reference ~ Simple).+
      Simple               := Underscore | StringInterpolation | Literal | Reference | Product | Id | Sequence
       Underscore          := `__` ~ Type.Assignment.?
       StringInterpolation := Value.Reference ~ `"` ~ (Interpolation ~ !`"`) ~ `"` // "
       Interpolation       := Expression.InterpolationEscape | `S` ~ Id | `S` ~ `{` ~ Pattern ~ `}`
       Reference           := Value.Reference ~ Product
       Product             := `(` ~ Pattern ~ (`,` ~ Pattern).* ~`)`
       Sequence            := `__` ~ `*`
  }
}
