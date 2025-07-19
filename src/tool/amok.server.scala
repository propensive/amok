package amok

import soundness.{is as _, Node as _, *}

def httpServer()(using Stdio): Unit raises ServerError raises ClasspathError = tcp"8080".serve:
  request.location match
    case _ /: t"api.css"  => Http.Response(Classpath/"amok"/"api.css")
    case _ /: t"utils.js" => Http.Response(Classpath/"amok"/"utils.js")
    case _ /: t"logo.svg" => Http.Response(Classpath/"amok"/"logo.svg")

    case _ /: t"entity" /: (name: Text) =>
      val (symbol, entity, node) = model.resolve(name)

      Http.Response:
        import html5.*
        try
          recover:
            case MarkdownError(_) | CodlError(_, _, _, _) | CodlReadError(_) =>
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

                  Div
                   (H2(Code(kind.syntax.html, t" ", entity, t" extends ".unless(kind.extensions.isEmpty), exts)),
                    if node.types.isEmpty then P(Em(t"No type members."))
                    else Table.members:
                      node.types.to(List).flatMap: (name, template) =>
                        val link: Path on Rfc3986 = (% / "entity" / index.child(name, true).id).on[Rfc3986]
                        List
                         (Tr(Td.kind(Code(template.definition.let(_.syntax.html))),
                             Th(Code(A(href = link)(name)))),
                          template.memo.let { memo => Tr(Td, Td.memo(memo.html)) })),

                node.definition.let: kind =>
                  Div
                   (H2(Code.typed
                     (kind.syntax.html,
                      t" ",
                      Em(entity),
                      node.params.let(_.html),
                      t": ".unless(node.returnType.absent),
                      node.returnType.let(_.html))),
                    if node.terms.isEmpty then P(Em(t"No term members."))
                    else Table.members:
                      node.terms.to(List).flatMap: (name, term) =>
                        val link: Path on Rfc3986 = (% / "entity" / index.child(name, false).id).on[Rfc3986]
                        List
                         (Tr(Td.kind(Code(term.definition.let(_.syntax).let(_.html))),
                             Th(Code(A(href = link)(name)))),
                          term.memo.let { memo => Tr(Td, Td.memo(memo.html)) })),
                detail.let(_.html).let(Div(_)))

        catch
          case exception: Throwable =>
            Page.simple(H1(t"Error"), Div(exception.stackTrace.html))

    case _ /: t"api" =>
      import html5.*
      val rootLocation: Path on Rfc3986 = % / "entity"

      Http.Response:
        Page
         (Nil,
          List
           (H2(t"All Packages"),
            Ul.all
             (model.root.members.filter(!_(1).hidden).map: (member, _) =>
                val link: Path on Rfc3986 = (% / "api" / member.text.skip(1)).on[Rfc3986]
                Li(Code(A(href = link)(member.text.skip(1)))))))

    case _ /: t"api" /: (pkg: Text) =>
      import html5.*

      val rootLocation: Path on Rfc3986 = % / "entity" / pkg

      Http.Response:
        Page
         (List
           (Details.imports
             (Summary(B(t"import")),
              Div(Details(Summary(t"scala.*"))),
              Div(Details(Summary(t"scala.Predef.*"))),
              Div(Details(Summary(t"scala.collection.*")))),
            Details(Summary(B(A(target = id"main", href = rootLocation)(pkg)))),
            Div:
              model(pkg).members.filter(!_(1).hidden).map: (member, node) =>
                node.tree(member.text, pkg, pkg+member.safe)),
          List(Iframe(id = id"api", name = t"main", src = rootLocation, width = 700)))

    case _ =>
      Http.Response(t"Hello")
