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

import soundness.{Span as _, *}

export executives.completions
export backstops.stackTraceBackstop
export interpreters.simpleInterpreter
export threading.platformThreading
export workingDirectories.daemonClientWorkingDirectory
export logging.silentLogging
export filesystemOptions.dereferenceSymlinks.{enabled as dereferencingEnabled}
export filesystemOptions.readAccess.{enabled as readAccessEnabled}
export filesystemOptions.writeAccess.{enabled as writeAccessEnabled}
export filesystemOptions.createNonexistent.{enabled as creationEnabled}
export filesystemOptions.createNonexistentParents.{enabled as parentCreationEnabled}
export alphabets.base64Standard
export treeStyles.defaultTreeStyle
export httpServers.stdlibHttpServer
export probates.cancelProbate
export supervisors.globalSupervisor
export charEncoders.utf8Encoder
export charDecoders.utf8Decoder
export textSanitizers.skipSanitizer
export classloaders.threadContextClassloader
export errorDiagnostics.stackTracesDiagnostics
export proximities.levenshteinProximity
export caseSensitivity.caseInsensitive
export temporaryDirectories.javaTemporaryDirectory
export environments.daemonClientEnvironment as defaultEnvironment
export systems.javaSystem as defaultSystem

export Definition.*

import doms.html.whatwg, whatwg.*

given minimal: scintillate.WebserverErrorPage = (request, throwable) =>
  import hieroglyph.charEncoders.utf8Encoder
  Http.Response(Unfulfilled(t"An error occurred which prevented the request from completing."))

given pathOnWwwAttributive: (Path on Www) is Attributive to honeycomb.Whatwg.Url =
  (key, value) => (key, value.encode)

given targetAttributive: Target is Attributive to honeycomb.Whatwg.Target =
  (key, value) => (key, value.show)

case class RootPackage(member: Item)

given memberIsRenderable: (imports: Imports, mountpoint: Mountpoint, model: Model, root: RootPackage)
      => (Item is Renderable { type Form = Phrasing }) =
  new Renderable:
    type Self = Item
    type Form = Phrasing

    def render(member: Item): Html of Phrasing =
      def recur(member: Item): Html of Phrasing =
        def link(name: Text): Html of Phrasing =
          if model.root(member) == root.member then A(href = mountpoint / "_entity" / member.encode)(name)
          else A(target = Target.Top, href = mountpoint / "_api" / member.encode)(name)

        member match
          case Item(Unset, name) => link(name)

          case Item(parent: Typename, name) =>
            if imports.has(parent) then if model.has(member) then link(member.name) else member.name
            else Span(Span(recur(parent.member)), member.symbol, if model.has(member) then link(name) else name)

      recur(member)

given divisible: Typename is Divisible by Text to Item = Item(_, _)

extension (typename: Typename)
  def member: Item = typename match
    case Typename.Top(name)          => Item(Unset, name)
    case Typename.Term(parent, name) => Item(parent, name)
    case Typename.Type(parent, name) => Item(parent, name)

given translator: Tactic[TelError] => Tactic[ParseError]
      => (Model, RootPackage, Mountpoint, Imports)
      => Translator =
  new Translator:
    def translate(nodes: Seq[Markdown]): Seq[Html of Flow] = Nil
    def phrasing(node: Markdown of Prose): Seq[Html of Phrasing] = Nil

given syntaxIsRenderable
:   ( imports: Imports, mountpoint: Mountpoint, model: Model, root: RootPackage )
=>  (Syntax is Renderable { type Form = Phrasing }) = new Renderable:
  type Self = Syntax
  type Form = Phrasing

  def render(syntax: Syntax): Html of Phrasing =
    def txt(t: Text): Html of Phrasing = t
    def sep(items: List[Html of Phrasing], delim: Text): List[Html of Phrasing] =
      items match
        case Nil          => Nil
        case head :: Nil  => head :: Nil
        case head :: tail => head :: tail.flatMap(item => List(txt(delim), item))

    syntax match
      case Syntax.Simple(typename)                          => typename.member.html
      case Syntax.Symbolic(text)                            => txt(text)
      case Syntax.Projection(Syntax.Simple(typename), text) => render(Syntax.Simple(Typename.Type(typename, text)))
      case Syntax.Projection(base, text)                    => Fragment[Phrasing](render(base), txt(t"⌗"), txt(text))
      case Syntax.Primitive(text)                           => txt(text)
      case Syntax.Selection(left, right)                    => Fragment[Phrasing](render(left), txt(t"."), txt(right))
      case Syntax.Prefix(prefix, base)                      => Fragment[Phrasing](txt(prefix), txt(t" "), render(base))
      case Syntax.Suffix(base, suffix)                      => Fragment[Phrasing](render(base), txt(suffix))

      case Syntax.Sequence(open, elements) =>
        val (l, r) = open match
          case '(' => (t"(", t")")
          case '[' => (t"[", t"]")
          case '{' => (t"{", t"}")
          case _   => (t"", t"")
        Fragment[Phrasing](txt(l) :: sep(elements.map(render).to(List), t", ") ::: txt(r) :: Nil *)

      case Syntax.Value(typename)        => Fragment[Phrasing](typename.member.html, txt(t".type"))
      case Syntax.Compound(syntaxes)     => Fragment[Phrasing](syntaxes.map(render).to(List)*)

      case Syntax.Declaration(method, syntaxes, result) =>
        Fragment[Phrasing](syntaxes.map(render).to(List) ::: txt(if method then t": " else t"") :: render(result) :: Nil *)

      case Syntax.Application(left, elements, infix) => left match
        case Syntax.Simple(Typename.Type(parent, name)) if infix && imports.has(parent) =>
          Fragment[Phrasing](render(elements(0)), txt(t" "), txt(name), txt(t" "), render(elements(1)))

        case _ =>
          Fragment[Phrasing](render(left) :: txt(t"[") :: sep(elements.map(render).to(List), t", ") ::: txt(t"]") :: Nil *)

      case Syntax.Structural(base, members, defs) =>
        val members2 = members.map { (name, syntax) => s"type $name = ${syntax.text}".tt }
        val defs2 = defs.map { (name, syntax) => s"def $name${syntax.text}".tt }
        Fragment[Phrasing](render(base), txt(s" { ${(members2 ++ defs2).mkString("; ")} }".tt))

      case Syntax.Infix(left, middle, right) =>
        val left2 = if left.precedence < syntax.precedence then Syntax.Sequence('(', List(left)) else left
        val right2 = if right.precedence < syntax.precedence then Syntax.Sequence('(', List(right)) else right
        Fragment[Phrasing](render(left2), txt(t" "), txt(middle), txt(t" "), render(right2))

      case Syntax.Named(isUsing, name, inner) =>
        Fragment[Phrasing](txt(if isUsing then t"using " else t""), txt(name), txt(t": "), render(inner))
