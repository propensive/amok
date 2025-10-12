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

class JvmFolio(mountpoint: Mountpoint, source: Path on Linux, var model: Model = Model())
extends Folio(mountpoint, t"jvm", source):

  given Mountpoint = mountpoint

  def handle(using Http.Request): Http.Response = subpath match
    case _ /: t"api.css"     => unsafely(Http.Response(Classpath / "amok" / "api.css"))
    case _ /: t"code.css"    => unsafely(Http.Response(Classpath / "amok" / "code.css"))
    case _ /: t"utils.js"    => unsafely(Http.Response(Classpath / "amok" / "utils.js"))
    case _ /: t"navigate.js" => unsafely(Http.Response(Classpath / "amok" / "navigate.js"))
    case _ /: t"logo.svg"    => unsafely(Http.Response(Classpath / "amok" / "logo.svg"))

    case _ /: t"_entity" /: (name: Text) =>
      val (symbol, entity, node) = model.resolve(name)

      Http.Response:
        import html5.*
        recover:
          case MarkdownError(_) | CodlError(_) | ParseError(_, _, _) =>
            Page.simple(mountpoint, H2(t"Error"), P(t"The page contained errors"))

        . within:
            import Markdown.renderable

            val detail: Optional[Markdown[Markdown.Ast.Block]] =
              node.detail.let(Markdown.parse(_))

            val index = Index.decode(name)
            val prnt = index.only { case Index.Entity(parent, isType, entity) => parent }.option

            given Imports(Set
                   (Index.decode(t"scala.Predef"),
                    Index.decode(t"scala.collection"),
                    Index.decode(t"scala.collection.immutable"),
                    Index.decode(t"scala"),
                    Index.decode(t"prepositional"),
                    Index.decode(t"java.lang")) ++ prnt)


            Page.simple
             (mountpoint, index.only:
                case Index.Entity(parent, isType, entity) =>
                  H1.pkg(Code(parent.html, symbol)),

              H1(Code(entity)),
              node.template.let: kind =>
                val exts =
                  if kind.extensions.length == 0 then Unset
                  else Syntax.sequence(kind.extensions, Syntax.Comma).let(_.html)

                val colon = if node.types.isEmpty then Nil else List(Code(t":"))

                Table.members
                 (Tr(Th(colspan = 6)(Code(kind.syntax.html), Code(t" "), Code(Em(entity), t" extends ".unless(kind.extensions.isEmpty), exts), colon)),
                  node.types.to(List).flatMap: (name, template) =>
                    val link: Path on UrlSpace = (mountpoint / "_entity" / index.child(name, true).id).on[UrlSpace]

                    val templateEntry: Optional[Html["tr"]] =
                      template.template.let: template =>
                        Tr(Td.kind(Code(template.syntax.html)), Th(colspan = 5)(Code(A(href = link)(name))))

                    val definitionEntry: Optional[Html["tr"]] =
                      template.definition.let: definition =>
                        Tr(Td.kind(Code(definition.syntax.html)), Th(colspan = 5)(Code(A(href = link)(name))))

                    List
                     (templateEntry,
                      definitionEntry,
                      template.memo.let { memo => Tr(Td, Td.memo(memo.html)) }).compact),

              node.definition.let: kind =>
                val colon = if node.terms.isEmpty then Nil else List(Code(t":"))

                Table.members
                 (Tr
                   (Th(colspan = 6)
                     (Code(kind.syntax.html), Code(t" "), Code.typed
                     (Em(entity),
                      node.params.let(_.html),
                      t": ".unless(node.returnType.absent),
                      node.returnType.let(_.html),
                      colon))),

                  if false
                  then List(Tr(Td(colspan = 6)(Em(t"This term has no members."))))
                  else

                    val grouped: Seq[Html["tr"]] =
                      node.terms.to(List).flatMap: (name, node) =>
                        node.definition match
                          case `extension`(subject, definition, modifiers) =>
                            List((subject, definition.syntax, name, node.memo))
                          case _ =>
                            Nil
                      . groupBy(_(0)).to(List).flatMap: (subject, defs) =>
                          List
                           (List(Tr(Td.extension(Code(Span(t"extension"))), Td.extension2(colspan = 5)(Code(subject.html)))),
                            defs.flatMap: (_, term, name, memo) =>
                              val link = (mountpoint / "_entity" / index.child(name, false).id).on[UrlSpace]

                              List
                               (Tr(Td.kind(Code(term.html)), Th(colspan = 5)(Code(A(href = link)(name)))),
                                memo.let { memo => Tr(Td(colspan = 6), Td.memo(memo.html)) }))
                          . flatten

                    val members: Seq[Html["tr"]] = node.terms.to(List).flatMap: (name, term) =>
                      term.definition match
                        case `extension`(_, _, _) => Nil
                        case definition =>
                          val link: Path on UrlSpace = (mountpoint / "_entity" / index.child(name, false).id).on[UrlSpace]

                          List
                           (Tr(Td.kind(Code(definition.let(_.syntax).let(_.html))),
                            Th(colspan = 5)(Code(A(href = link)(name)))),
                            term.memo.let { memo => Tr(Td, Td.memo(colspan = 5)(memo.html)) })

                    members ++ grouped),

              detail.let(_.html).let(Div(_)))

    case _ /: t"_api" =>
      import html5.*
      val rootLocation: Path on UrlSpace = mountpoint / "_entity"

      Http.Response:
        Page
         (mountpoint,
          Nil,
          List
           (H2(t"All Packages"),
            Ul.all
             (model.root.members.filter(!_(1).hidden).map: (member, _) =>
                val link: Path on UrlSpace = (mountpoint / "_api" / member.text.skip(1)).on[UrlSpace]
                Li(Code(A(href = link)(member.text.skip(1)))))))

    case _ /: t"_api" /: (pkg: Text) =>
      import html5.*

      val rootLocation: Path on UrlSpace = mountpoint / "_entity" / pkg

      Http.Response:
        Page
         (mountpoint,
          List
           (Details.imports
             (Summary(B(t"import")),
              Div(Details(Summary(t"scala.*"))),
              Div(Details(Summary(t"scala.Predef.*"))),
              Div(Details(Summary(t"scala.collection.*")))),
            Details(Summary(H3(A(target = id"main", href = rootLocation)(pkg)))),
            Div:
              model(pkg).members.filter(!_(1).hidden).map: (member, node) =>
                node.tree(member.text, pkg, pkg+member.safe)),
          List(Iframe(id = id"api", name = t"main", src = rootLocation)))

    case _ =>
      Server.at(request.location).let(_.handle(using request)).or:
        Http.Response(NotFound(t"Not found"))
