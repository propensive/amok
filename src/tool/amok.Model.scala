                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                               ╭───╮                              ┃
┃                                                               │   │                              ┃
┃                                                               │   │                              ┃
┃                     ╭─────────╮ ╭───╮╌────╮╌────╮ ╭─────────╮ │   │ ╭───╮                        ┃
┃                     ╰─────╮   │ │   ╭─╮   ╭─╮   │ │   ╭─╮   │ │   │╌╯   │                        ┃
┃                     ╭─────╯   │ │   │ │   │ │   │ │   │ │   │ │        ╌╯                        ┃
┃                     │   ╭─╮   │ │   │ │   │ │   │ │   │ │   │ │   ╭─╮   │                        ┃
┃                     │   ╰─╯   │ │   │ │   │ │   │ │   ╰─╯   │ │   │ │   │                        ┃
┃                     ╰─────────╯ ╰───╯ ╰───╯ ╰───╯ ╰─────────╯ ╰───╯ ╰───╯                        ┃
┃                                                                                                  ┃
┃    Amok, prerelease version                                                                      ┃
┃    © Copyright 2023-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://github.com/propensive/amok/                                                       ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package amok

import scala.tasty.*, inspector.*
import scala.quoted.*

import soundness.{is as _, open as _, Node as _, *}

extension (using Quotes)(flags: quotes.reflect.Flags)
  def modifier(modifier: Modifier): Boolean =
    import quotes.reflect.*
    modifier match
      case `case`        => flags.is(Flags.Case)
      case `private`     => flags.is(Flags.Private)
      case `protected`   => flags.is(Flags.Protected)
      case `abstract`    => flags.is(Flags.Abstract) || flags.is(Flags.AbsOverride)
      case `open`        => flags.is(Flags.Open)
      case `final`       => flags.is(Flags.Final)
      case `erased`      => flags.is(Flags.Erased)
      case `transparent` => flags.is(Flags.Transparent)
      case `inline`      => flags.is(Flags.Inline)
      case `lazy`        => flags.is(Flags.Lazy)
      case `sealed`      => flags.is(Flags.Sealed)
      case `override`    => flags.is(Flags.Override) || flags.is(Flags.AbsOverride)
      case `opaque`      => flags.is(Flags.Opaque)
      case `infix`       => flags.is(Flags.Infix)
      case `into`        => false
      case `tracked`     => false

  def has(modifiers: Modifier*): List[Modifier] = modifiers.filter(flags.modifier(_)).to(List)

class Model():
  val root = Node()

  def resolve(path: Text, prefix: Text = t"", current: Node = root, term: Boolean = true)
  : (Text, Text, Node) =

      path.where { char => char == '.' || char == ':' }.let: position =>
        val part = path.before(position).urlDecode
        val next = current() = if term then Member.OfTerm(part) else Member.OfType(part)

        path.at(position) match
          case '.' => resolve(path.after(position), t"$prefix$part.", next, true)
          case ':' => resolve(path.after(position), t"$prefix$part⌗", next, false)
          case _   => (if prefix.empty then t"" else prefix.keep(1, Rtl), part, current)

      . or:
         (if prefix.empty then t"" else prefix.keep(1, Rtl),
          path.urlDecode,
          current() = if term then Member.OfTerm(path.urlDecode) else Member.OfType(path.urlDecode))

  def apply(pkg: Text): Node = root() = Member.OfTerm(pkg)

  def overlay(base: Amox.Base)(using Stdio): Unit =
    def recur(prefix: Text, entries: List[Amox.Entry], current: Node): Unit =
      entries.map: entry =>
        val part = entry.name.skip(1)
        val next = entry.name.at(Prim) match
          case '.' => current() = Member.OfTerm(part)
          case '#' => current() = Member.OfType(part)
          case other   => Out.println(m"Unexpected: ${other.inspect}") yet Unset

        next.let: next =>
          next.memo = entry.memo.dare(_.read[InlineMd])
          next.detail = entry.detail
          next.hidden = entry.hidden.or(false)
          recur(prefix+entry.name, entry.entry, next)

    val init = root() = Member.OfTerm(base.base.or(t""))
    init.memo = base.memo.dare(_.read[InlineMd])
    init.detail = base.detail
    recur(base.base.or(t""), base.entry, init)

  def load(path: Path on Linux)(using Stdio): Unit =
    val inspector = DocInspector()
    try TastyInspector.inspectTastyFilesInJar(path.encode.s)(inspector)
    catch case error: Throwable =>
      Out.println(error.stackTrace.teletype)

    Syntax.clear()
    java.lang.System.gc()

  import Member.{OfTerm, OfType}

  case class DocInspector()(using Stdio) extends Inspector:
    def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
      import quotes.reflect.*
      import Flags.*

      def ref(term: Tree): Optional[Typename] = term match
        case Ident(name)        => Typename.Top(name)
        case Select(term, name) => ref(term).let(Typename.Term(_, name))
        case This(_)            => Unset

      def walk(ast: Tree, node: Node, ofTerm: Boolean): Unit =
        def of(name: Text): Member = if ofTerm then OfTerm(name) else OfType(name)
        ast match
          case pc@PackageClause(id@Ident(name), body) =>
            val child = node() = of(name)
            child() = `package`(Nil)
            body.each(walk(_, child, true))

          case valDef@ValDef(name, result, body) if !(valDef.symbol.flags.is(Private) || name == "_") =>
            val flags = valDef.symbol.flags
            val termName =
              if flags.is(Given) && (name.tt.starts(t"evidence$$"))
              then name.tt/*Syntax(result.tpe)*/ else name.tt
            if name.tt.ends(t"$$package") then body.each(walk(_, node, true))
            else
              val child = node() = of(termName)
              val returnType = Syntax(result.tpe)

              if flags.is(Enum) && flags.is(Case)
              then child() = Template.`case`(flags.has(`private`), Nil, Unset)
              else if flags.is(Synthetic) then Unset
              else if flags.is(Given)
              then child() = `given`(flags.has(`inline`, `transparent`, `erased`), returnType)
              else if flags.is(Module)
              then child() = `object`(flags.has(`case`))
              else if flags.is(Mutable)
              then child() = `var`(flags.has(`override`, `private`, `protected`, `final`), returnType)
              else child() = `val`(flags.has(`override`, `private`, `protected`, `erased`, `inline`, `final`, `lazy`), returnType)

              body.each(walk(_, child, true))

          case classDef@ClassDef(name, DefDef(_, groups0, _, _), extensions0, selfType, body) =>
            val typeRef = classDef.symbol.typeRef
            val flags = classDef.symbol.flags
            val parents = typeRef.baseClasses.map(typeRef.baseType(_))

            given TypeRepr is PartiallyOrdered = (left, right) =>
              !(left =:= right) && left <:< right

            val caseClass = flags.is(Case)
            val dag = Poset(parents*).dag

            val extensions: List[Syntax] =
              dag(parents.head).filter: repr =>
                repr.typeSymbol != defn.ObjectClass
                && repr.typeSymbol != defn.AnyClass
                && repr.typeSymbol != defn.AnyRefClass
                && (caseClass && repr.typeSymbol != defn.ProductClass)
                && (caseClass && repr.typeSymbol != TypeRepr.of[java.io.Serializable].typeSymbol)

              . to(List)
              . map(Syntax(_))

            val obj = flags.is(Module)
            val className = if obj && name.tt.ends(t"$$") then name.tt.skip(1, Rtl) else name.tt
            val child = node() = if flags.is(Enum) then OfType(name) else of(className)

            if name.tt.ends(t"$$package") || name.tt.ends(t"$$package$$")
            then body.each(walk(_, node, true))
            else
              val params = if groups0.isEmpty && flags.is(Trait) then Unset else Syntax.Compound(groups0.map(Syntax.clause(_, true)))
              if flags.is(Synthetic) || flags.is(Module) then ()
              else if flags.is(Trait)
              then child() = Template.`trait`(flags.has(`private`, `erased`, `into`, `sealed`, `transparent`), extensions, params)
              else if obj && !flags.is(Case) && flags.is(Enum)
              then child() = Template.`enum`(flags.has(`private`), Nil, Nil, params)
              else if flags.is(Case) && flags.is(Enum)
              then child() = Template.`case`(flags.has(`private`, `protected`), extensions, params)
              else if flags.is(Case)
              then child() = Template.`case class`(flags.has(`private`, `protected`, `sealed`, `open`, `transparent`, `final`, `into`, `erased`, `abstract`), extensions, Nil, params)
              else child() = Template.`class`(flags.has(`private`, `protected`, `sealed`, `open`, `transparent`, `final`, `into`, `erased`, `abstract`), extensions, params, Nil)

              body.each(walk(_, child, obj))



          case term@DefDef(name, groups0, result, body)
            if !term.symbol.flags.is(Synthetic) && !term.symbol.flags.is(Private)
               && !name.contains("$default$") =>

            val flags = term.symbol.flags
            val isGiven = flags.is(Given)
            val isInfix = flags.is(Infix)
            val termName = name.show
            val child = node() = of(termName)
            val ext = flags.is(ExtensionMethod)
            val split = 1 + groups0.indexWhere:
              case clause@TermParamClause(terms) =>
                terms.length == 1 && !terms.exists(_.symbol.flags.is(Given))
              case _ => false

            val preClauses = if ext then groups0.take(split) else Nil
            val paramClauses = if ext then groups0.drop(split) else groups0

            val params =
              if isGiven then Unset
              else Syntax.Compound(Syntax.Symbolic(if isInfix then " " else "") :: paramClauses.map(Syntax.clause(_, true)))


            val returnType =
              if isGiven then
                Syntax.Compound(paramClauses.flatMap: clause =>
                  List(Syntax.clause(clause, false), Syntax.Symbolic(t" => "))
                :+ Syntax(result.tpe))
              else Syntax(result.tpe)
            child() =
              if isGiven then `given`(flags.has(`inline`, `transparent`, `erased`), returnType)
              else
                val definition: amok.Definition.`def` =
                  `def`(flags.has(`abstract`, `override`, `private`, `protected`, `erased`, `final`, `infix`, `transparent`, `inline`), params, returnType)

                if ext then `extension`(Syntax.Compound(preClauses.map(Syntax.clause(_, true))), definition)
                else definition


          case typeDef@TypeDef(name, result) if name != "MirroredMonoType" =>
            val flags = typeDef.symbol.flags
            val typeName = name.show
            def child = node() = of(typeName)

            if !flags.is(Flags.Param)
            then child() = Template.`type`(flags.has(`into`, `opaque`, `infix`), Nil, Unset)

          case Export(term, exports) => exports.map:
            case SimpleSelector(name)    =>
              node(of(name.tt))
            case RenameSelector(_, name) => node(of(name.tt))
            case other: Selector         => Out.println(t"Found a different kind of selector")

          case other if other.symbol.flags.is(Private) =>
          case other  =>

      tastys.each: tasty =>
        walk(tasty.ast, root, true)
