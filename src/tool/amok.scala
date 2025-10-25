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

given typenameIsRenderable: (imports: Imports, mountpoint: Mountpoint) => Typename is Renderable:
  import html5.*
  type Result = Phrasing

  def html(index: Typename): List[Html[Phrasing]] =
    def recur(index: Typename): Html[Phrasing] =
      val ref = index.id
      def link(name: Text) = A(href = mountpoint / "_entity" / ref)(name)

      index match
        case Typename.Top(name) =>
          Span(link(name))

        case Typename.Term(_, name) if name.starts(t"_$$") =>
          Span(name.skip(2))

        case Typename.Type(_, name) if name.starts(t"_$$") =>
          Span(name.skip(2))

        case Typename.Term(parent, name) =>
          if imports.has(parent) then Span(link(name)) else Span(recur(parent), t".", link(name))

        case Typename.Type(parent, name) =>
          if imports.has(parent) then Span(link(name)) else Span(recur(parent), t"⌗", link(name))

    List(recur(index))

given translator: Tactic[CodlError] => Tactic[ParseError] => (model: Model) => Translator =
  new HtmlTranslator(AmokEmbedding(false), ScalaEmbedding):
    override def phrasing(node: Markdown.Ast.Inline): Seq[Html[html5.Phrasing]] = node match
      case Markdown.Ast.Inline.SourceCode(code) =>
        List:
          html5.Code:
            if code.starts(t".") then t"$code"
            else if code.starts(t"#") then model.resolve(code.skip(1)) match
              case (_, _, node) => node.template.let(_.syntax.html)
            else code

      case other => super.phrasing(other)

given syntaxIsRenderable: (imports: Imports, mountpoint: Mountpoint) => Syntax is Renderable:
  import html5.*
  type Result = Phrasing

  def html(syntax: Syntax): List[Html[Phrasing]] = List:
    syntax match
      case Syntax.Simple(typename)       => Span(typename.text)
      case Syntax.Symbolic(text)         => Span(text)
      case Syntax.Project(base, text)    => Span(s"${base.text}#$text".tt)
      case Syntax.Constant(text)         => Span(text)
      case Syntax.Selection(left, right) => Span(s"${left.text}.${right}")
      case Syntax.Prefix(prefix, base)   => Span(s"$prefix ${base.text}".tt)
      case Syntax.Suffix(base, suffix)   => Span(s"${base.text}$suffix".tt)
      case Syntax.Tuple(false, elements) => Span(s"(${elements.map(_.text).mkString(", ")})".tt)
      case Syntax.Tuple(true, elements)  => Span(s"[${elements.map(_.text).mkString(", ")}]".tt)
      case Syntax.Singleton(typename)    => Span(s"${typename.text}.type".tt)
      case Syntax.Compound(syntaxes)     => Span(syntaxes.map(_.text).mkString.tt)

      case Syntax.Signature(method, syntaxes, result) =>
        Span(s"${syntaxes.map(_.text).mkString}${if method then ": " else ""}${result.text}".tt)

      case Syntax.Application(left, elements, infix) => left match
        case Syntax.Simple(Typename.Type(parent, name)) if infix && imports.has(parent) =>
          Span(Syntax.Infix(elements(0), name, elements(1)).text)

        case _ =>
          Span(left.text+elements.map(_.text).mkString("[", ", ", "]").tt)

      case Syntax.Refined(base, members, defs) =>
        val members2 = members.map { (name, syntax) => s"type $name = ${syntax.text}".tt }
        val defs2 = defs.map { (name, syntax) => s"def $name${syntax.text}".tt }
        Span(s"${base.text} { ${(members2 ++ defs2).mkString("; ")} }".tt)

      case Syntax.Infix(left, middle, right) =>
        val left2 = if left.precedence < syntax.precedence then Syntax.Tuple(false, List(left)) else left
        val right2 = if right.precedence < syntax.precedence then Syntax.Tuple(false, List(right)) else right
        Span(s"${left2.text} $middle ${right2.text}".tt)

      case Syntax.Named(isUsing, name, syntax) =>
        Span(if isUsing then s"using $name: ${syntax.text}".tt else s"$name: ${syntax.text}".tt)
