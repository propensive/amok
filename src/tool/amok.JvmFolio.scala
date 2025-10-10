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

class JvmFolio(mountpoint: Mountpoint, source: Text, var model: Model = Model())
extends Folio(mountpoint, t"jvm", source):

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
            Page.simple(H2(t"Error"), P(t"The page contained errors"))

        . within:
            import Markdown.renderable

            val detail: Optional[Markdown[Markdown.Ast.Block]] =
              node.detail.let(Markdown.parse(_))

            val index = Index.decode(name)

            Page.simple
             (index.only:
                case Index.Entity(parent, isType, entity) =>
                  H1.pkg(Code(parent.html, symbol)),

              H1(Code(entity)),
              node.template.let: kind =>
                val exts =
                  if kind.extensions.length == 0 then Unset
                  else Syntax.sequence(kind.extensions, Syntax.Comma).let(_.html)

                Table.members
                 (Tr(Th(Code(kind.syntax.html)), Th(colspan = 2)(Code(entity, t" extends ".unless(kind.extensions.isEmpty), exts))),
                  if node.types.isEmpty then Tr(Td(colspan = 3)((Em(t"This type has no members."))))
                  else node.types.to(List).flatMap: (name, template) =>
                    val link: Relative on UrlSpace = (? / "_entity" / index.child(name, true).id).on[UrlSpace]
                    List
                      (Tr(Td,
                          Td.kind(Code(template.definition.let(_.syntax.html))),
                          Th(Code(A(href = link)(name)))),
                      template.memo.let { memo => Tr(Td(colspan = 2), Td.memo(memo.html)) })),

              node.definition.let: kind =>
                Table.members
                 (Tr
                   (Th.kind(Code(kind.syntax.html)),
                    Th(colspan = 2)
                     (Code.typed
                     (Em(entity),
                      node.params.let(_.html),
                      t": ".unless(node.returnType.absent),
                      node.returnType.let(_.html)))),

                  if node.terms.isEmpty
                  then List(Tr(Td(colspan = 3)(Em(t"This term has no members."))))
                  else
                    val extensions: List[(Syntax, Syntax, Text, Optional[InlineMd])] =
                      node.terms.to(List).flatMap: (name, node) =>
                        node.definition match
                          case `extension`(subject, definition, modifiers) =>
                            List((subject, definition.syntax, name, node.memo))
                          case _ =>
                            Nil

                    val grouped: Seq[Html["tr"]] =
                      extensions.groupBy(_(0)).to(List).flatMap: (subject, defs) =>
                        List
                         (List(Tr(Th, Th(colspan = 2)(Code(t"extension ", subject.html)))),
                          defs.flatMap: (_, term, name, memo) =>
                            val link: Relative on UrlSpace = (? / "_entity" / index.child(name, false).id).on[UrlSpace]

                            List
                             (Tr(Td, Td.kind(Code(term.html)), Th(Code(A(href = link)(name)))),
                              memo.let { memo => Tr(Td(colspan = 2), Td.memo(memo.html)) }))
                        . flatten

                    val members: Seq[Html["tr"]] = node.terms.to(List).flatMap: (name, term) =>
                      term.definition match
                        case `extension`(_, _, _) => Nil
                        case definition =>
                          val link: Relative on UrlSpace = (? / "_entity" / index.child(name, false).id).on[UrlSpace]

                          List
                           (Tr(Td, Td.kind(Code(definition.let(_.syntax).let(_.html))),
                            Th(Code(A(href = link)(name)))),
                            term.memo.let { memo => Tr(Td(colspan = 2), Td.memo(memo.html)) })

                    members ++ grouped),

              detail.let(_.html).let(Div(_)))

    case _ /: t"_api" =>
      import html5.*
      val rootLocation: Relative on UrlSpace = ? / "_entity"

      Http.Response:
        Page
         (Nil,
          List
           (H2(t"All Packages"),
            Ul.all
             (model.root.members.filter(!_(1).hidden).map: (member, _) =>
                val link: Relative on UrlSpace = (? / "_api" / member.text.skip(1)).on[UrlSpace]
                Li(Code(A(href = link)(member.text.skip(1)))))))

    case _ /: t"_api" /: (pkg: Text) =>
      import html5.*

      val rootLocation: Relative on UrlSpace = ? / "_entity" / pkg

      Http.Response:
        Page
         (List
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
