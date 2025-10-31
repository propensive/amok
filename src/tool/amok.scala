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

import soundness.{is as _, Node as _, *}

export executives.completions
export unhandledErrors.stackTrace
export interpreters.posix
export threading.platform
export workingDirectories.daemonClient
export logging.silent
export filesystemOptions.dereferenceSymlinks.{enabled as dereferencingEnabled}
export filesystemOptions.readAccess.{enabled as readAccessEnabled}
export filesystemOptions.writeAccess.{disabled as writeAccessDisabled}
export filesystemOptions.createNonexistent.{disabled as creationDisabled}
export alphabets.base64.standard
export treeStyles.default
export httpServers.stdlibPublic
export codicils.cancel
export supervisors.global
export charEncoders.utf8
export charDecoders.utf8
export textSanitizers.skip
export classloaders.threadContext
export webserverErrorPages.stackTraces

import html5.*

case class RootPackage(member: Member)

given memberIsRenderable: (imports: Imports, mountpoint: Mountpoint, model: Model, root: RootPackage)
      => Member is Renderable:
  type Result = Phrasing

  def html(member: Member): List[Html[Phrasing]] =
    def recur(member: Member): Html[Phrasing] =
      def link(name: Text): Html[Phrasing] =
        if model.root(member) == root.member then A(href = mountpoint / "_entity" / member.encode)(name)
        else A(target = id"_top", href = mountpoint / "_api" / member.encode)(name)

      member match
        case Member(Unset, name) => link(name)

        case Member(parent: Typename, name) =>
          if imports.has(parent) then if model.has(member) then link(member.name) else member.name
          else Span(Span(recur(parent.member)), member.symbol, if model.has(member) then link(name) else name)

    List(recur(member))

given divisible: Typename is Divisible by Text to Member = Member(_, _)

extension (typename: Typename)
  def member: Member = typename match
    case Typename.Top(name)          => Member(Unset, name)
    case Typename.Term(parent, name) => Member(parent, name)
    case Typename.Type(parent, name) => Member(parent, name)

given translator: Tactic[CodlError] => Tactic[ParseError]
      => (Model, RootPackage, Mountpoint, Imports)
      => Translator =
  new HtmlTranslator(AmokEmbedding(false), ScalaEmbedding):
    override def phrasing(node: Markdown.Ast.Inline): Seq[Html[Phrasing]] = node match
      case Markdown.Ast.Inline.SourceCode(code) =>
        List:
          Code:
            if code.starts(t".") then t"$code"
            else if code.starts(t"#") then
              Typename(code.skip(1)).member.html
            else code

      case other => super.phrasing(other)

given syntaxIsRenderable: (imports: Imports, mountpoint: Mountpoint, model: Model, root: RootPackage)
      => Syntax is Renderable:
  type Result = Phrasing

  def html(syntax: Syntax): Seq[Html[Phrasing]] = syntax match
    case Syntax.Simple(typename)                          => typename.member.html
    case Syntax.Symbolic(text)                            => List(text)
    case Syntax.Projection(Syntax.Simple(typename), text) => html(Syntax.Simple(Typename.Type(typename, text)))
    case Syntax.Projection(base, text)                    => base.html :+ t"⌗" :+ text
    case Syntax.Primitive(text)                           => List(text)
    case Syntax.Selection(left, right)                    => left.html :+ t"." :+ right
    case Syntax.Prefix(prefix, base)                      => prefix +: t" " +: base.html
    case Syntax.Suffix(base, suffix)                      => base.html :+ suffix

    case Syntax.Sequence('(', elements) =>
      t"(" +: elements.flatMap(_.html :+ t", ").dropRight(1) :+ t")"

    case Syntax.Sequence('[', elements)  =>
      t"[" +: elements.flatMap(_.html :+ t", ").dropRight(1) :+ t"]"

    case Syntax.Sequence('{', elements)  =>
      t"{" +: elements.flatMap(_.html :+ t", ").dropRight(1) :+ t"}"

    case Syntax.Value(typename)    => typename.member.html :+ ".type"
    case Syntax.Compound(syntaxes)     => syntaxes.flatMap(_.html)

    case Syntax.Declaration(method, syntaxes, result) =>
      syntaxes.flatMap(_.html) ++ ((if method then t": " else t"") +: result.html)

    case Syntax.Application(left, elements, infix) => left match
      case Syntax.Simple(Typename.Type(parent, name)) if infix && imports.has(parent) =>
        elements(0).html ++ (t" " +: name +: t" " +: elements(1).html)

      case _ =>
        left.html ++ (t"[" +: elements.flatMap(_.html :+ t", ").dropRight(1) :+ t"]")

    case Syntax.Structural(base, members, defs) =>
      val members2 = members.map { (name, syntax) => s"type $name = ${syntax.text}".tt }
      val defs2 = defs.map { (name, syntax) => s"def $name${syntax.text}".tt }
      base.html :+ s" { ${(members2 ++ defs2).mkString("; ")} }".tt

    case Syntax.Infix(left, middle, right) =>
      val left2 = if left.precedence < syntax.precedence then Syntax.Sequence('(', List(left)) else left
      val right2 = if right.precedence < syntax.precedence then Syntax.Sequence('(', List(right)) else right
      left2.html ++ (t" " +: middle +: t" " +: right2.html)

    case Syntax.Named(isUsing, name, syntax) =>
      (if isUsing then t"using " else t"") +: name +: t": " +: syntax.html
