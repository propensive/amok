package amok

import scala.tasty.*, inspector.*
import scala.quoted.*
import scala.collection.mutable as scm

import soundness.{is as _, Node as _, *}

object Page:
  import html5.*

  def apply(nav: Html[Flow]*): HtmlDoc = HtmlDoc:
    Html
     (Head
       (Script(src = % / "utils.js", defer = true), Link.Stylesheet(href = % / "api.css")),
      Body
       (Header(Ul
          (Li(A(href = % / "api")(t"API")),
           Li(A(href = % / "tutorials")(t"TUTORIALS")),
           Li(A(href = % / "glossary")(t"GLOSSARY")),
           Li(Button(id = id"theme")))),
        Main
         (Nav(Div.menu(nav*)),
          Article(Iframe(id = id"api", name = t"main", width = 700))),
        Footer(t"© Copyright 2025, Propensive OÜ")))

  def simple(content: Html[Flow]*): HtmlDoc = HtmlDoc:
    Html(Head(Link.Stylesheet(href = % / "styles.css")), Body(content*))
