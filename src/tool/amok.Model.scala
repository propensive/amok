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
  val root = Node(Unset)

  def resolve(path: Text, prefix: Text = t"", current: Node = root, term: Boolean = true)
  : Optional[(Text, Text, Node)] =

      path.where { char => char == '.' || char == ':' }.let: position =>
        val part = path.before(position).urlDecode
        val next = current(if term then Member.OfTerm(part) else Member.OfType(part))

        path.at(position) match
          case '.' => next.let(resolve(path.after(position), t"$prefix$part.", _, true))
          case ':' => next.let(resolve(path.after(position), t"$prefix$part⌗", _, false))
          case _   => (if prefix.empty then t"" else prefix.keep(1, Rtl), part, current)

      . or:
          val path2 = path.urlDecode
          current(if term then Member.OfTerm(path2) else Member.OfType(path2)).let: current =>
            (if prefix.empty then t"" else prefix.keep(1, Rtl), path2, current)

  def apply(pkg: Text): Optional[Node] = root(Member.OfTerm(pkg))

  def overlay(base: Amox.Base)(using Stdio): Unit =
    def recur(prefix: Text, entries: List[Amox.Entry], current: Node): Unit =
      entries.map: entry =>
        val part = entry.name.skip(1)
        val next = entry.name.at(Prim) match
          case '.'   => current(Member.OfTerm(part))
          case '#'   => current(Member.OfType(part))
          case other => Out.println(m"Unexpected: ${other.inspect}") yet Unset

        next.let: next =>
          next.memo = entry.memo.dare(_.read[InlineMd])
          next.detail = entry.detail
          next.hidden = entry.hidden.or(false)
          recur(prefix+entry.name, entry.entry, next)

    root.memo = base.memo.dare(_.read[InlineMd])
    root.detail = base.detail
    recur(base.base.or(t""), base.entry, root)

  def load(path: Path on Linux)(using Stdio, Terminal): Unit =
    val inspector = DocInspector()
    try TastyInspector.inspectTastyFilesInJar(path.encode.s)(inspector)
    catch case error: Throwable =>
      Out.println(error.stackTrace.teletype)

    Syntax.clear()
    java.lang.System.gc()

  import Member.{OfTerm, OfType}

  case class DocInspector()(using Stdio, Terminal) extends Inspector:
    def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
      import quotes.reflect.*
      import Flags.*


      def walk(tree: Tree, node: Node, ofTerm: Boolean, typename0: Optional[Typename] = Unset): Unit =
        val flags = tree.symbol.flags

        tree.match
          case PackageClause(Ident(name), body) => (name.tt, body.to(List), true)
          case ClassDef(name, _, _, _, body)    => (name.tt, body.to(List), flags.is(Module))
          case DefDef(name, _, _, body)         => (name.tt, Nil, true)
          case ValDef(name, _, body)            => (name.tt, body.to(List), true)
          case TypeDef(name, _)                 => (name.tt, Nil, false)
          case other                            => Unset

        . let: (name0, body, term) =>
            val name2 = name0.when(_.ends("$"))(_.skip(1, Rtl))
            val packageObject = name2.ends("$package")
            val name = name2.when(packageObject)(_.skip(8, Rtl))

            val typename = tree match
              case PackageClause(_, _) => Typename(tree.symbol.fullName)
              case _ =>
                typename0.lay(Typename.Top(name)): typename =>
                  if term then Typename.Term(typename, name) else Typename.Type(typename, name)

            def of(name: Text): Member = if ofTerm then OfTerm(name) else OfType(name)

            if name != "_" then
              tree match
                case tree@PackageClause(packageName, stats) =>
                  val child = node(name) = of(name)
                  child() = `package`(Nil)
                  body.each(walk(_, child, true, typename))

                case tree@ValDef(_, result, _) if !tree.symbol.flags.is(Private) =>
                  if packageObject then
                    body.each(walk(_, node, ofTerm, typename))
                  else
                    val child = node(name) = of(name)
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

                    body.each(walk(_, child, true, typename))

                case tree@ClassDef(_, DefDef(_, groups0, _, _), extensions0, selfType, _) =>
                  val typeRef = tree.symbol.typeRef
                  val obj = flags.is(Module)
                  val parents = typeRef.baseClasses.map(typeRef.baseType(_))

                  Out.println(typename.render)

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

                  if packageObject then body.each(walk(_, node, ofTerm, typename))
                  else
                    val child = node(name) = if flags.is(Enum) then OfType(name) else of(name)
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

                    body.each(walk(_, child, obj, typename))



                case tree@DefDef(_, groups0, result, _)
                  if !tree.symbol.flags.is(Synthetic) && !tree.symbol.flags.is(Private) =>

                  val isGiven = flags.is(Given)
                  val isInfix = flags.is(Infix)
                  val termName = name.show
                  val child = node(name) = of(termName)
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


                case tree@TypeDef(name, result) if name != "MirroredMonoType" =>
                  def child = node(name) = of(name)

                  if !flags.is(Flags.Param)
                  then child() = Template.`type`(flags.has(`into`, `opaque`, `infix`), Nil, Unset)

                case Export(term, exports) => exports.map:
                  case SimpleSelector(name)    =>
                    node(of(name.tt))
                  case RenameSelector(_, name) => node(of(name.tt))
                  case other: Selector         => Out.println(t"Found a different kind of selector")

                case other if other.symbol.flags.is(Private) =>
                case other  =>

      val total = tastys.length
      Out.println(t"There are $total TASTy files")
      var count = 0
      val t0 = java.lang.System.currentTimeMillis
      val grades = "▏▎▍▌▋▊▉█"

      val columns = summon[Terminal].columns.or(100)
      tastys.each: tasty =>
        val percent = 8*columns*count/total
        val bar = e"${t"█"*(percent/8)}${grades(percent%8)}"
        // Out.print(e"\r${Bg(rgb"#221100")}(${rgb"#DD9900"}($bar)${t" "*(columns - percent/8 - 1)})")
        walk(tasty.ast, root, true, Unset)
        count += 1

      Out.println(((java.lang.System.currentTimeMillis - t0)/1000.0).toString)
