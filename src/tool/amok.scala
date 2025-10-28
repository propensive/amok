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
        case Typename.Top(name) => link(name)

        case child =>
          if imports.has(child.parent) then link(child.name)
          else Span(Span(recur(child.parent)), index.symbol("⌗"), link(child.name))

    List(recur(index))

given translator: Tactic[CodlError] => Tactic[ParseError] => (model: Model) => Translator =
  new HtmlTranslator(AmokEmbedding(false), ScalaEmbedding):
    override def phrasing(node: Markdown.Ast.Inline): Seq[Html[html5.Phrasing]] = node match
      case Markdown.Ast.Inline.SourceCode(code) =>
        List:
          html5.Code:
            if code.starts(t".") then t"$code"
            else if code.starts(t"#") then model.resolve(code.skip(1)) match
              case (_, _, node) => node.template.let(_.syntax().html)
            else code

      case other => super.phrasing(other)

given syntaxIsRenderable: (imports: Imports, mountpoint: Mountpoint) => Syntax is Renderable:
  import html5.*
  type Result = Phrasing

  def html(syntax: Syntax): Seq[Html[Phrasing]] = syntax match
    case Syntax.Simple(typename)       => typename.html
    case Syntax.Symbolic(text)         => List(text)
    case Syntax.Project(Syntax.Simple(typename), text) => html(Syntax.Simple(Typename.Type(typename, text)))
    case Syntax.Project(base, text)    => base.html :+ t"⌗" :+ text
    case Syntax.Constant(text)         => List(text)
    case Syntax.Selection(left, right) => left.html :+ t"." :+ right
    case Syntax.Prefix(prefix, base)   => prefix +: t" " +: base.html
    case Syntax.Suffix(base, suffix)   => base.html :+ suffix

    case Syntax.Tuple(false, elements) =>
      t"(" +: elements.flatMap(_.html :+ t", ").dropRight(1) :+ t")"

    case Syntax.Tuple(true, elements)  =>
      t"[" +: elements.flatMap(_.html :+ t", ").dropRight(1) :+ t"]"

    case Syntax.Singleton(typename)    => typename.html :+ ".type"
    case Syntax.Compound(syntaxes)     => syntaxes.flatMap(_.html)

    case Syntax.Signature(method, syntaxes, result) =>
      syntaxes.flatMap(_.html) ++ ((if method then t": " else t"") +: result.html)

    case Syntax.Application(left, elements, infix) => left match
      case Syntax.Simple(Typename.Type(parent, name)) if infix && imports.has(parent) =>
        elements(0).html ++ (t" " +: name +: t" " +: elements(1).html)

      case _ =>
        left.html ++ (t"[" +: elements.flatMap(_.html :+ t", ").dropRight(1) :+ t"]")

    case Syntax.Refined(base, members, defs) =>
      val members2 = members.map { (name, syntax) => s"type $name = ${syntax.text}".tt }
      val defs2 = defs.map { (name, syntax) => s"def $name${syntax.text}".tt }
      base.html :+ s" { ${(members2 ++ defs2).mkString("; ")} }".tt

    case Syntax.Infix(left, middle, right) =>
      val left2 = if left.precedence < syntax.precedence then Syntax.Tuple(false, List(left)) else left
      val right2 = if right.precedence < syntax.precedence then Syntax.Tuple(false, List(right)) else right
      left2.html ++ (t" " +: middle +: t" " +: right2.html)

    case Syntax.Named(isUsing, name, syntax) =>
      (if isUsing then t"using " else t"") +: name +: t": " +: syntax.html
