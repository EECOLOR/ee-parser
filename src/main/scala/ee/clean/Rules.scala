package ee.clean

class Scope[T](val scope:T)

object Rules extends Rules(Global) {
  import Definition.Operations
  import scope._, Metadata.Decorations

  TopLevelStatements := (PackageStatement | Import | Template | Member).*
  Statements         := (                   Import | Template | Member | Expression).*

  PackageStatement   := Decorations ~ `package` ~ Value.Reference ~ !`{`

  new Scope(Import) { import scope._
    Import             := Decorations ~ `import` ~ Expression ~ (`,` ~ Expression).*
      Expression       := Single | All | Selectors
      Single           := Value.Reference
      All              := Value.Reference ~ `.` ~ `_`
      Selectors        := `{` ~ Selector ~ (`,` ~ Selector).* ~ `}`
      Selector         := SimpleSelector | RenamingSelector
      SimpleSelector   := Value.Reference | `_`
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
    Body          := `{` ~ Self.? ~ Statements ~ `}`
      Self        := (`_` | Id) ~ Type.Assignment.? ~ `=>`
    New           := `new` ~ (Parents | Parents ~ Body | Body)
  }

  new Scope(Member) { import scope._
    Member := Decorations ~ (Type | Def | Val | Var) ~ Value.Assignment.?
      Type :=  `type` ~ Id ~ Type.Parameters.* ~ Type.Constraint.*
      Def  := `def` ~ Id ~ Type.Parameters.* ~ Value.Parameters.* ~ Type.Assignment.? 
      Val  := `val` ~ Ids ~ (`,` ~ Ids).*
      Var  := `var` ~ Ids ~ (`,` ~ Ids).*
      Ids  := (Id ~ Type.Assignment.?) | Pattern
  }

  new Scope(Type) { import scope._
    Assignment            := `:` ~ (Reference | ExpandVariableArity)
    Parameters            := `[` ~ Parameter ~ (`,` ~ Parameter).* ~ `]`
      Parameter           := Decorations ~ Variance.? ~ (Id | `_`) ~ Parameters.? ~ Constraint.*
      Variance            := `covariant` | `contravariant`
      Constraint          := (`<:` | `>:` | `<%` | `:`) ~ Reference
    Application           := `[` ~ Reference ~ (`,` ~ Reference) ~ `]`
    Reference             := Infix | Single | Structural | VariableArity
      Infix               := Reference ~ Reference ~ Reference
      Single              := Reference ~ Projection.*
      Structural          := Template.Body
      VariableArity       := Single ~ `*`
      Projection          := `#` ~ Single
      ExpandVariableArity := `_` ~ `*`
  }

  new Scope(Value) { import scope._
    Assignment             := `=` ~ (`macro` ~ Reference | Expression)
    Parameters             := `(` ~ Parameter ~ (`,` ~ Parameter).* ~ `)`
      Parameter            := Member
    Reference              := UnqualifiedReference ~ (`.` ~ UnqualifiedReference).*
      UnqualifiedReference := Id ~ Type.Application.*
    Application            := Expression.Group
  }

  new Scope(Metadata) { import scope._
    Decorations       := Annotation.* ~ Modifier.*
      Modifier        := AccessModifier | SpecialModifier
      AccessModifier  := (`public` | `private` | `protected`) ~ AccessQualifier.?
      AccessQualifier := `[` ~ Value.Reference ~ (`,` ~ Value.Reference).* ~ `]`
      SpecialModifier :=  `implicit` | `override` | `abstract` | `package` | `case`
  }

  new Scope(Expression) { import scope._
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
        TickEmbedded   := `'''` ~ Id ~ !`'''` ~ `'''`

      Lambda           := (Value.Parameters | Value.Parameter) ~ `=>` ~ Expression

      Compound         := (Infix | Prefix | Simple) ~ Postfix.? ~ Suffix.?
        Prefix         := PrefixOperator ~ Simple
        Infix          := (Prefix | Simple) ~ (Value.UnqualifiedReference ~ (Prefix | Simple)).+
        Postfix        := Value.UnqualifiedReference
        Suffix         := Match | Type.Assignment | Value.Assignment

      Simple           := Group | Primitive

      Group            := Product | Cases | Block
        Block          := `{` ~ Statements ~ `}`
        Product        := `(` ~ Expression ~ (`,` ~ Expression).* ~ `)`
        Cases          := `{` ~ Case.+ ~ `}`
        Case           := `case` ~ Pattern ~ (`|` ~ Pattern).* ~ Guard.? ~ `=>` ~ Statements

      Primitive             := Literal | StringInterpolation | Value.Reference | `_`
        StringInterpolation := Value.Reference ~ `"` ~ (Interpolation | !`"`).+ ~ `"`
        Interpolation       := InterpolationEscape | `$` ~ Id | `$` ~ Block
        InterpolationEscape := `$` ~ `$`

  }
}

abstract class Rules[T](t:T) extends Scope(t) {
  private var _rules = Set.empty[(Rule, Definition)]
  def rules = _rules

  implicit def addRule(rule: Rule, definition: Definition): Unit =
    _rules += rule -> definition
}