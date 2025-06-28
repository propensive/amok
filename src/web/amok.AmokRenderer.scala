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

import html5.*

object AmokEmbedding:
  def formatCode(samples0: List[Text], context: Optional[Scala.Context]): Html["div"] =
    val samples: List[List[List[SourceToken]]] =
      samples0.map(Scala.highlight(_, context).lines.to(List))

    val name = t"f${counter()}"
    val lineCount = samples.map(_.length).max
    val maxWidth = samples0.flatMap(_.cut(t"\n")).map(_.length).max
    val fontSize = (97.5/maxWidth).min(3.0)
    given Decimalizer(decimalPlaces = 3)

    def similar(xs0: List[SourceToken], ys0: List[SourceToken]): Boolean =
      val xs = xs0.filter(_.accent != Accent.Unparsed)
      val ys = ys0.filter(_.accent != Accent.Unparsed)
      xs.length > 0 && ys.length > 0
      && (xs.sliding(ys.length).contains(ys) || ys.sliding(xs.length).contains(xs))
      || diff(IArray.from(xs), IArray.from(ys)).size.toDouble/(xs.length + ys.length) < 0.5

    val iterators: IArray[Iterator[List[SourceToken]]] = IArray.from(samples.map(_.iterator))

    val lines: List[List[List[SourceToken]]] = evolve(samples, similar).sequence.map: atom =>
      (0 until samples.length).to(List).map: lineNo =>
        if !atom.presence.contains(lineNo.z) then Nil else
          val line = iterators(lineNo).next()
          if line.all(_.text.s.forall(_.isWhitespace)) then List(SourceToken(t" ", Accent.Unparsed))
          else line

    val radios =
      (0 until samples.length).map: sample =>
         html5.Input.Radio
          (`class` = List(CssClass(t"s$sample")), name = name, checked = sample == 0)
      . unless(samples.length <= 1)

    val code =
      lines.map: line =>
        evolve(line).sequence.map: atom =>
          val classes = atom.presence.to(List).map { step => CssClass(t"v${step.n0}") }
          html5.Code
           (`class` = classes ::: ScalaEmbedding.className(atom.value.accent),
            style = t"width: ${atom.value.text.length}ch")(atom.value.text)
        :+ html5.Code(`class` = List(CssClass(t"blank")))
      . map: line =>
          html5.Span.line(line)

    html5.Div.amok(style = t"font-size: ${fontSize}vw")(radios, html5.Pre(code))

class AmokEmbedding()(using Tactic[CodlError], Tactic[CodlReadError]) extends Embedding(t"amok"):
  def render(meta: Optional[Text], content: Text): Seq[Html["div"]] =
    val preamble = Codl.read[Preamble](content)

    val context = preamble.context match
      case t"term" => Scala.Context.Term
      case t"type" => Scala.Context.Type
      case _       => Scala.Context.Top

    val code: Text =
      val lines = content.cut(t"\n").to(List).dropWhile(_ != t"##")
      if lines.length > 0 then lines.tail.join(t"\n") else t""

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
          line:   List[Element[Phrasing]]  = Nil,
          done:   List[Element[Phrasing]]  = Nil)
    :     List[Element[Phrasing]] =
      import html5.*
      def render(token: SourceToken | Note): Element[Phrasing] = token match
        case SourceToken(text, accent) =>
          ScalaEmbedding.element(accent, text)

        case Note(tokens, style, caption) =>
          val captionSpan =
            caption.lay(Nil): text =>
              val textLines = text.cut(t"\n")
              val height = textLines.length*1.5
              val width = textLines.map(_.length).max + 1
              List(Span.caption(style = t"width:${width}ch;height:${height}em")(Span(text)))

          val content =
            captionSpan
            ++ tokens.reverse.map { token => ScalaEmbedding.element(token.accent, token.text) }

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
            Scala.highlight(text, context).lines.to(List).flatMap(SourceToken.Newline :: _)))
        . init
        . tail

    List(AmokEmbedding.formatCode(preamble.version.map(_.content), context))
