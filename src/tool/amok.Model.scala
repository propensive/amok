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
import scala.collection.mutable as scm

import soundness.{is as _, open as _, Node as _, *}

import environments.jre

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
  private val roots: scm.HashSet[Member] = scm.HashSet()
  private val index: scm.HashMap[Member, Node] = scm.HashMap()

  def root(member: Member): Optional[Member] =
    if roots.contains(member) then member else member.parent.let(_.member).let(root(_))

  def establish(typename: Typename): Node =
    val member = Member(typename.parent, typename.name)
    index.at(member).or:
      Node().tap: node =>
        index(member) = node

        typename.parent match
          case Unset       => roots += typename.member
          case parent: Typename => establish(parent).add(typename.member)

  def packages: List[Member] = roots.to(List).sortBy(_.encode)
  def lookup(typename: Typename): Optional[Node] = index.at(Member(typename.parent, typename.name))
  def lookup(member: Member): Optional[Node] = index.at(member)
  def has(member: Member): Boolean = index.contains(member)

  def overlay(amox: Amox.Base)(using Stdio): Unit =
    val root = Member(Unset, amox.base)

    def recur(entries: List[Amox.Entry], focus0: Member): Unit =
      entries.each: entry =>
        val focus: Optional[Member] = entry.name.at(Prim) match
          case '.'  => Member(focus0.definition, entry.name.skip(1))
          case '#'  => Member(focus0.template, entry.name.skip(1))
          case char => Out.println(t"Unexpected entry: ${char.or('?')}") yet Unset

        focus.let: focus =>
          lookup(focus).let: node =>
            node.state.info = entry.info.dare(_.read[InlineMd])
            node.state.document = entry.detail
            node.state.hidden = entry.hidden.or(false)

          recur(entry.entry, focus)

    lookup(root).let: node =>
      node.state.info = amox.info.dare(_.read[InlineMd])
      node.state.document = amox.detail

    recur(amox.entry, root)

  def load(path: Path on Linux)(using Stdio, Terminal): Unit =
    val inspector = DocInspector()
    try TastyInspector.inspectTastyFilesInJar(path.encode.s)(inspector)
    catch case error: Throwable =>
      Out.println(error.stackTrace.teletype)

    Syntax.clear()
    java.lang.System.gc()

  case class DocInspector()(using Stdio, Terminal) extends Inspector:
    def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
      import quotes.reflect.*
      import Flags.*


      def walk(tree: Tree, typename0: Optional[Typename] = Unset): Unit =
        val flags = tree.symbol.flags

        tree.match
          case PackageClause(Ident(name), body) => (name.tt, body.to(List), true)
          case ClassDef(name, _, _, _, body)    => (name.tt, body.to(List), flags.is(Module))
          case DefDef(name, _, _, body)         => (name.tt, body.to(List), true)
          case ValDef(name, _, body)            => (name.tt, body.to(List), true)
          case TypeDef(name, _)                 => (name.tt, Nil, false)
          case other                            => Unset

        . let: (name0, body, term) =>
            val module = name0.ends("$")
            val name2 = name0.when(module)(_.skip(1, Rtl))
            val packageObject = name2.ends("$package")
            val name = name2.when(packageObject)(_.skip(8, Rtl))

            val typename: Typename = tree match
              case PackageClause(_, _) =>
                Typename(tree.symbol.fullName)

              case _ =>
                if packageObject then typename0.or(Typename.Top(name))
                else typename0.lay(Typename.Top(name)): typename =>
                  if module then Typename.Term(typename, name) else Typename.Type(typename, name)

            if name != "_" && (!flags.is(Synthetic) || flags.is(Module)) && !name.contains("$default$") then
              val node: Node = establish(typename)
              if name0.contains("rudiments") then Out.println(t"    ${node.toString}")

              tree match
                case tree@PackageClause(packageName, _) =>
                  body.each(walk(_, typename))

                case tree@ValDef(_, result, _) if !tree.symbol.flags.is(Private) =>
                  val returnType = Syntax(result.tpe)

                  if flags.is(Enum) && flags.is(Case)
                  then node.declare(Template.`case`(flags.has(`private`), Nil, Unset))
                  else if flags.is(Given)
                  then node.declare(`given`(flags.has(`inline`, `transparent`, `erased`), returnType))
                  else if flags.is(Module)
                  then node.declare(`object`(flags.has(`case`)))
                  else if flags.is(Mutable)
                  then node.declare(`var`(flags.has(`override`, `private`, `protected`, `final`), returnType))
                  else node.declare(`val`(flags.has(`override`, `private`, `protected`, `erased`, `inline`, `final`, `lazy`), returnType))

                  body.each(walk(_, typename))

                case tree@ClassDef(_, DefDef(_, groups0, _, _), _, _, _) =>
                  val typeRef = tree.symbol.typeRef
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

                  val params =
                    if groups0.isEmpty && flags.is(Trait) then Unset
                    else Syntax.Compound(groups0.map(Syntax.clause(_, true)))

                  if flags.is(Module)
                  then node.declare(`object`(flags.has(`case`)))
                  else if flags.is(Trait)
                  then node.declare(Template.`trait`(flags.has(`private`, `erased`, `into`, `sealed`, `transparent`), extensions, params))
                  else if flags.is(Module) && !flags.is(Case) && flags.is(Enum)
                  then node.declare(Template.`enum`(flags.has(`private`), Nil, Nil, params))
                  else if flags.is(Case) && flags.is(Enum)
                  then node.declare(Template.`case`(flags.has(`private`, `protected`), extensions, params))
                  else if flags.is(Case)
                  then node.declare(Template.`case class`(flags.has(`private`, `protected`, `sealed`, `open`, `transparent`, `final`, `into`, `erased`, `abstract`), extensions, Nil, params))
                  else node.declare(Template.`class`(flags.has(`private`, `protected`, `sealed`, `open`, `transparent`, `final`, `into`, `erased`, `abstract`), extensions, params, Nil))

                  body.each(walk(_, typename))

                case tree@DefDef(_, groups0, result, _) =>

                  val isGiven = flags.is(Given)
                  val isInfix = flags.is(Infix)
                  val termName = name.show
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

                  if isGiven then node.declare(`given`(flags.has(`inline`, `transparent`, `erased`), returnType))
                  else
                    val definition: `def` =
                      `def`(flags.has(`abstract`, `override`, `private`, `protected`, `erased`, `final`, `infix`, `transparent`, `inline`), params, returnType)

                    if ext then node.declare(`extension`(Syntax.Compound(preClauses.map(Syntax.clause(_, true))), definition))
                    else node.declare(definition)

                case tree@TypeDef(name, result) =>
                  if !flags.is(Flags.Param)
                  then node.declare(Template.`type`(flags.has(`into`, `opaque`, `infix`), Nil, Unset))
                  else node.declare(Template.TypeParam(flags.has(`into`, `opaque`, `infix`), Nil, Unset))

                case Export(term, exports) => exports.map:
                  case SimpleSelector(name)    => //node(of(name.tt))
                  case RenameSelector(_, name) => //node(of(name.tt))
                  case other: Selector         =>

                case other if other.symbol.flags.is(Private) =>
                case other  =>
                  Out.println(t"Skipping ${other.getClass.toString}")

      val total = tastys.length
      Out.println(t"There are $total TASTy files")
      var count = 0
      val t0 = java.lang.System.currentTimeMillis
      val grades = "▏▎▍▌▋▊▉█"

      val columns = summon[Terminal].columns.or(safely(Environment.columns)).or(100)
      tastys.each: tasty =>
        val percent = 8*columns*count/total
        val bar = e"${t"█"*(percent/8)}${grades(percent%8)}"
        Out.print(e"\r\e[?7l${Bg(rgb"#221100")}(${rgb"#DD9900"}($bar)${t" "*(columns - percent/8 - 1)})\e[?7h")
        walk(tasty.ast, Unset)
        count += 1

      for root <- roots do
        val node = lookup(root).vouch
        val size = node.members.size
        if size == 1 then
          roots.remove(root)
          roots += node.members.head
