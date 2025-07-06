package amok

import scala.quoted.*
import scala.collection.mutable as scm

import soundness.{is as _, Node as _, *}

enum Typus:
  case Simple(address: Address)
  case Compound(precedence: Int, typus: Typus*)
  case Symbolic(text: Text)
  case Constant(text: Text)
  case Member(text: Text)
  case Refined(typus: Typus, members: ListMap[Text, Typus])

object Typus:
  val OpenParens = Typus.Symbolic(t"(")
  val CloseParens = Typus.Symbolic(t")")
  val OpenBracket = Typus.Symbolic(t"\u200b[")
  val CloseBracket = Typus.Symbolic(t"]")
  val Space = Typus.Symbolic(t" ")
  val Intersect = Typus.Symbolic(t" & ")
  val Union = Typus.Symbolic(t" | ")
  val Comma = Typus.Symbolic(t", ")
  val Colon = Typus.Symbolic(t": ")
  val Arrow = Typus.Symbolic(t"=> ")
  val FunctionArrow = Typus.Symbolic(t" => ")
  val ContextualArrow = Typus.Symbolic(t" ?=> ")
  val LambdaArrow = Typus.Symbolic(t" =>> ")
  val Qmark = Typus.Symbolic(t"?")
  val Dot = Typus.Symbolic(t".")
  val Project = Typus.Symbolic(t"#")
  val LowerBound = Typus.Symbolic(t"? >: ")
  val UpperBound = Typus.Symbolic(t"? <: ")
  val AndUpperBound = Typus.Symbolic(t" <: ")

  def apply(outer: Int, typus: Typus*): Typus.Compound =
    val elements = typus.flatMap:
      case Typus.Compound(inner, elements*) =>
        if inner < outer then OpenParens +: elements :+ CloseParens else elements
      case refinement: Refined => Seq(OpenParens, refinement, CloseParens)
      case other                  => Seq(other)

    Typus.Compound(outer, elements*)

  given renderable: Imports => Typus is Renderable:
    import html5.*
    type Result = Phrasing

    def html(typus: Typus): List[Html[Phrasing]] = List:
      typus match
        case Simple(address)         => Span(address.html)
        case Compound(_, typuses*)   => Span(typuses.flatMap(html(_)))
        case Symbolic(text)          => Span(text)
        case Member(text)            => Span(text)
        case Constant(text)          => Span(text)
        case Refined(typus, members) => Span(html(typus), t"{ ... }")

  given showable: Imports => Typus is Showable:
    def text(typus: Typus): Text = typus match
      case Simple(address)        => address.text
      case Compound(_, typuses*)  => typuses.map(text(_)).join
      case Symbolic(text)         => text
      case Member(text)           => text
      case Constant(text)         => text
      case Refined(base, members) => t"$base { ... }"

  val cache: scm.HashMap[Any, Typus] = scm.HashMap()

  def precedence(char: Char): Int = char match
    case '!'  => 4
    case '%'  => 8
    case '&'  => 3
    case '*'  => 8
    case '+'  => 7
    case '-'  => 7
    case '/'  => 8
    case ':'  => 6
    case '<'  => 5
    case '='  => 4
    case '>'  => 5
    case '^'  => 2
    case '|'  => 1
    case char => if char.isLetter then 0 else 9

  def apply(using quotes: Quotes)(repr: quotes.reflect.TypeRepr, thisType: Boolean = false)(using Stdio): Typus = cache.establish(repr):
    import quotes.reflect.*

    repr.absolve match
      case ThisType(tpe) => apply(tpe, true)
      case TypeRef(NoPrefix() | ThisType(TypeRef(NoPrefix(), "<root>")), name) =>
        Typus.Simple(Address.Top(name))

      case TypeRef(prefix, name)   => apply(prefix) match
        case simple@Typus.Simple(address)    =>
          val obj = name.tt.ends(t"$$")
          val name2 = if obj then name.tt.skip(1, Rtl) else name.tt
          if name2.ends(t"$$package") then simple
          else Typus.Simple(Address.Entity(address, thisType, name2))

        case compound: Typus.Compound =>
          if compound.precedence < 10 then Typus(10, compound, Dot, Typus.Member(name.tt))
          else Typus(10, OpenParens, compound, CloseParens, Dot, Typus.Member(name.tt))

        case refined@Typus.Refined(base, members) =>
          if members.contains(name) then members(name.tt)
          else Typus(10, OpenParens, refined, CloseParens, Project, Typus.Member(name.tt))

        case other =>
          Out.println(t"OTHER: ${other.toString}") yet Typus.Constant(t"<unknown>")

      case TermRef(NoPrefix() | ThisType(TypeRef(NoPrefix(), "<root>")), name) =>
        Typus.Simple(Address.Top(name))

      case TermRef(prefix, name)   => apply(prefix) match
        case simple@Typus.Simple(address)    =>
          if name.tt.ends(t"$$package") then simple
          else Typus.Simple(Address.Entity(address, thisType, name))

        case compound: Typus.Compound =>
          if compound.precedence < 10 then Typus(10, compound, Dot, Typus.Member(name.tt))
          else Typus(10, OpenParens, compound, CloseParens, Dot, Typus.Member(name.tt))

        case refined@Typus.Refined(base, members) =>
          if members.contains(name) then members(name.tt)
          else Typus(10, OpenParens, refined, CloseParens, Project, Typus.Member(name.tt))

        case other =>
          Out.println(t"OTHER: ${other.toString}") yet Typus.Constant(t"<unknown>")

      case AnnotatedType(tpe, _) => apply(tpe) // FIXME
      case OrType(left, right)   => Typus(1, apply(left), Union, apply(right))
      case AndType(left, right)  => Typus(3, apply(left), Intersect, apply(right))
      case ByNameType(tpe)       => Typus(0, Arrow, apply(tpe))
      case FlexibleType(tpe)     => Typus(0, apply(tpe), Qmark)

      case AppliedType(base, args0) =>
        if args0.length == 2 && repr.typeSymbol.flags.is(Flags.Infix)
        then Typus(0, apply(args0(0)), Space, apply(base), Space, apply(args0(1)))
        else
          val args = args0.map(apply(_)).flatMap(List(_, Comma)).init
          if defn.isTupleClass(base.typeSymbol)
          then Typus(10, OpenParens +: args :+ CloseParens*)
          else Typus(10, apply(base) +: OpenBracket +: args :+ CloseBracket*)

      case ConstantType(constant) => constant match
        case IntConstant(int)       => Typus(10, Typus.Constant(int.show))
        case LongConstant(long)     => Typus(10, Typus.Constant(t"${long}L"))
        case BooleanConstant(true)  => Typus(10, Typus.Constant(t"true"))
        case BooleanConstant(false) => Typus(10, Typus.Constant(t"false"))
        case StringConstant(str)    => Typus(10, Typus.Constant(t"\"$str\""))
        case CharConstant(char)     => Typus(10, Typus.Constant(t"'$char'"))
        case DoubleConstant(double) => Typus(10, Typus.Constant(t"${double.toString}"))
        case FloatConstant(float)   => Typus(10, Typus.Constant(t"${float.toString}F"))
        case NullConstant()         => Typus(10, Typus.Constant(t"null"))

      case Refinement(base, name, member) => apply(base) match
        case Typus.Refined(base, members) =>
          Typus.Refined(base, members.updated(name, apply(member)))

        case other =>
          Typus.Refined(other, ListMap(name.tt -> apply(member)))

      case TypeBounds(lb, ub)                   =>
        if lb == ub then apply(lb)
        else if lb == TypeRepr.of[Nothing] && ub == TypeRepr.of[Any] then Typus(10, Qmark)
        else if lb == TypeRepr.of[Nothing] then Typus(10, UpperBound, apply(ub))
        else if lb == TypeRepr.of[Any] then Typus(10, LowerBound, apply(lb))
        else Typus(10, LowerBound, apply(lb), AndUpperBound, apply(ub))

      case method@MethodType(args0, types, result) =>
        val args =
          if args0.isEmpty then Nil
          else args0.zip(types).flatMap(Typus.Member(_) :: Colon :: apply(_) :: Comma :: Nil).init

        val arrow = if method.isContextFunctionType then ContextualArrow else FunctionArrow
        Typus(0, OpenParens +: args :+ CloseParens :+ arrow :+ apply(result)*)

      case PolyType(_, _, _) =>
        Typus.Constant(t"...poly method type...")

      case TypeLambda(args0, _, tpe)            =>
        val args = args0.map(Typus.Member(_)).flatMap(List(_, Comma)).init
        val params = (OpenBracket +: args :+ CloseBracket :+ LambdaArrow) :+ apply(tpe)
        Typus(0, params*)

      case ParamRef(binder, n) => binder match
        case TypeLambda(params, _, _) => Typus.Member(params(n))
        case MethodType(params, _, _) => Typus.Member(params(n))
        case other =>
          Out.println(t"Other kind of binder: ${other.toString}")
          Typus.Constant(t"ParamRef")

      case classInfo: dotty.tools.dotc.core.Types.ClassInfo =>
        val parents = classInfo.declaredParents.flatMap: tpe =>
          List(apply(tpe.asInstanceOf[TypeRepr]), Comma)
        Typus(0, parents.dropRight(1)*)


      // case PolyType(params0, types, result) =>
      //   TypeRender(t"<poly type>")


      // case ref: dotty.tools.dotc.core.Types.TypeParamRef => ref.binder match
      //   case TypeLambda(params, _, _) => TypeRender(params(ref.paramNum).tt)

      case other =>
        Out.println(t"Other kind of type: ${other.toString}")
        Typus.Constant(t"...other: ${other.toString}...")
