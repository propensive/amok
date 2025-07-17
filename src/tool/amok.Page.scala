package amok

import soundness.{is as _, Node as _, *}

object Page:
  import html5.*

  def apply(nav: List[Html[Flow]], article: List[Html[Flow]]): HtmlDoc =
    HtmlDoc:
      Html
       (Head
         (Script(src = % / "utils.js", defer = true), Link.Stylesheet(href = % / "api.css")),
        Body
         (Header(Ul
            (Li(A(href = % / "api")(t"API")),
             Li(A(href = % / "glossary")(t"GLOSSARY")),
             Li(A(href = % / "context")(t"CONTEXT")),
             Li(Button(id = id"theme")))),
          Main(Nav(Div.menu(nav*)), Article(article)),
          Footer(t"© Copyright 2025, Propensive OÜ")))

  def simple(content: Html[Flow]*): HtmlDoc = HtmlDoc:
    Html(Head(Link.Stylesheet(href = % / "api.css")), Body(content*))
