                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                               ╭───╮                              ┃
┃                                                               │   │                              ┃
┃                                                               │   │                              ┃
┃                        ╭─────────╮╭───╮╌────╮╌────╮╭─────────╮│   │ ╭───╮                        ┃
┃                        ╰─────╮   ││   ╭─╮   ╭─╮   ││   ╭─╮   ││   │╌╯   │                        ┃
┃                        ╭─────╯   ││   │ │   │ │   ││   │ │   ││        ╌╯                        ┃
┃                        │   ╭─╮   ││   │ │   │ │   ││   │ │   ││   ╭─╮   │                        ┃
┃                        │   ╰─╯   ││   │ │   │ │   ││   ╰─╯   ││   │ │   │                        ┃
┃                        ╰─────────╯╰───╯ ╰───╯ ╰───╯╰─────────╯╰───╯ ╰───╯                        ┃
┃                                                                                                  ┃
┃    Amok, version 0.1.0.                                                                          ┃
┃    © Copyright 2022-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://amok.propensive.com/                                                              ┃
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

import soundness.*

class AmokRenderer()(using Tactic[CodlError], Tactic[CodlReadError]) extends Renderer(t"amok"):
  def render(meta: Optional[Text], content: Text): Seq[Html[Flow]] =
    val preamble = Codl.read[Preamble](content)
    val code: Text = content.cut(t"\n").to(List).dropWhile(_ != t"##").tail.join(t"\n")

    val errorRanges = preamble.error.map(_.rangeIn(code, Note.Style.Erroneous))
    val cautionRanges = preamble.caution.map(_.rangeIn(code, Note.Style.Caution))
    val highlightRanges = preamble.highlight.map(_.rangeIn(code, Note.Style.Highlight))
    val paramRanges = preamble.param.map(_.rangeIn(code, Note.Style.Param))

    @tailrec
    def selections
         (ranges0:    List[Range],
          tokens0:    List[SourceToken],
          tokenStart: Int                      = -1,
          result:     List[SourceToken | Note] = Nil)
    :     List[SourceToken | Note] =

      ranges0 match
        case Nil =>
          result.unwind(tokens0)

        case range :: ranges =>
          tokens0 match
            case Nil =>
              result.reverse

            case token :: tokens =>
              val tokenEnd = tokenStart + token.length

              if tokenStart == range.start then
                if tokenEnd == range.end then
                  val result2 = result match
                    case Note(tokens, style, caption) :: more if style == range.style =>
                      Note(token :: tokens, range.style, caption) :: more

                    case other =>
                      Note(List(token), range.style, range.caption) :: result

                  selections(ranges, tokens, tokenStart + range.length, result2)

                else if tokenEnd < range.end
                then
                  val ranges2 = range.copy(end = tokenEnd) :: range.copy(start = tokenEnd) :: ranges
                  selections(ranges2, tokens0, tokenStart, result)
                else
                  val (leftToken, rightToken) = token.snip(range.length)
                  selections(ranges0, leftToken :: rightToken :: tokens, tokenStart, result)

              else if tokenStart < range.start then
                if tokenEnd <= range.start
                then selections(ranges0, tokens, tokenStart + token.length, token :: result)
                else
                  val (leftToken, rightToken) = token.snip(range.start - tokenStart)
                  selections(ranges0, leftToken :: rightToken :: tokens, tokenStart, result)

              else
                if range.end >= tokenStart
                then selections(ranges, tokens0, tokenStart, result)
                else panic(m"Should not happen: range.end=${range.end}, tokenStart=$tokenStart")

    def lines
       (markup: List[SourceToken | Note],
        line: List[Element[Phrasing]] = Nil,
        done: List[Element[Phrasing]] = Nil)
            : List[Element[Phrasing]] =
      import html5.*
      def render(token: SourceToken | Note): Element[Phrasing] = token match
        case SourceToken(text, accent) =>
          ScalaRenderer.element(accent, text)

        case Note(tokens, style, caption) =>
          val captionSpan =
            caption.lay(Nil): text =>
              val textLines = text.cut(t"\n")
              val height = textLines.length*1.5
              val width = textLines.map(_.length).max + 1
              List(Span.caption(style = t"width:${width}ch;height:${height}em")(Span(text)))

          val content =
            captionSpan
            ++ tokens.reverse.map { token => ScalaRenderer.element(token.accent, token.text) }

          style match
            case Note.Style.Erroneous => Span.err(content)
            case Note.Style.Caution   => Span.warn(content)
            case Note.Style.Highlight => Span.hi(content)
            case Note.Style.Param     => Span.param(content)

      markup match
        case Nil                         => done.unwind(List(Span.line(line.reverse)))
        case SourceToken.Newline :: tail => lines(tail, Nil, Span.line(line.reverse*) :: done)
        case other :: tail               => lines(tail, render(other) :: line, done)

    def style(text: Text): Element[Flow] =
      import html5.*
      val ranges = errorRanges ++ cautionRanges ++ highlightRanges ++ paramRanges

      Pre:
        lines
         (selections
           (ranges.compact.sortBy(_.start),
            Scala.highlight(text).lines.to(List).flatMap(SourceToken.Newline :: _)))
        . init
        . tail


    preamble.transform.lay(List(html5.Div.amok(style(code)))): transform =>
      val code2 = transform.replace.foldLeft(code) { (acc, transform) => transform(acc) }
      val differences = diff(Scala.highlight(code).lines, Scala.highlight(code2).lines)

      val output =
        differences
        . rdiff({ (left, right) => diff(left.to(Trie), right.to(Trie)).size < 5 }, 5).changes.map:
            case Par(_, _, line) => html5.Span.line:
              line.or(Nil).map { token => ScalaRenderer.element(token.accent, token.text) }

            case Sub(_, _, left, right) => html5.Span.line:
              diff(left.or(Nil).to(Trie), right.or(Nil).to(Trie)).edits.map:
                case Par(_, _, SourceToken(text, accent)) =>
                  html5.Code(`class` = ScalaRenderer.className(accent))(text)

                case Ins(_, SourceToken(text, accent)) =>
                  html5.Code
                   (`class` = CssClass(t"two") :: ScalaRenderer.className(accent),
                    style = t"width: ${text.length}ch")(text)

                case Del(_, SourceToken(text, accent)) =>
                  html5.Code
                   (`class` = CssClass(t"one") :: ScalaRenderer.className(accent),
                    style = t"width: ${text.length}ch")(text)

                case _ =>
                  panic(m"Should never have an unset edit")

            case Del(_, line) => html5.Span(`class` = List(CssClass(t"line"), CssClass(t"one"))):
              line.or(Nil).map { token => ScalaRenderer.element(token.accent, token.text) }

            case Ins(_, line) => html5.Span(`class` = List(CssClass(t"line"), CssClass(t"one"))):
              line.map { token => ScalaRenderer.element(token.accent, token.text) }

      val id = counter()

      import html5.*

      List
       (Div.amok
         (Input.Radio.fore(name = t"radiogroup_$id", id = DomId(t"before_$id"), checked = true),
          Label(`for` = DomId(t"before_$id"))(transform.before.or(t"Before")),
          Input.Radio.aft(name = t"radiogroup_$id", id = DomId(t"after_$id")),
          Label(`for` = DomId(t"after_$id"))(transform.after.or(t"After")),
          Pre(output.init)))
