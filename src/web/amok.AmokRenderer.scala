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

import soundness.{Token as _, Note as _, Span as _, *}
import harlequin.Token
import charEncoders.utf8Encoder

import doms.html.whatwg, whatwg.*

given (Unit is Tel.Decodable) = Tel.Decodable(Morphology.Empty)(_ => ())

object AmokEmbedding:
  def format(samples0: List[Text], context: Optional[Scala.Context], autoScale: Boolean)
  :   Html of Flow =

    val samples: List[List[List[Token]]] =
      samples0.map(Scala.highlight(_, context).lines.to(List))

    val name = t"f${counter()}"
    val lineCount = samples.map(_.length).max
    val maxWidth = samples0.flatMap(_.cut(t"\n")).map(_.length).max
    val fontSize = (97.5/maxWidth).min(3.0)
    given Decimalizer(decimalPlaces = 3)

    def similar(xs0: List[Token], ys0: List[Token]): Boolean =
      val xs = xs0.filter(_.accent != Accent.Unparsed)
      val ys = ys0.filter(_.accent != Accent.Unparsed)
      xs.length > 0 && ys.length > 0
      && (xs.sliding(ys.length).contains(ys) || ys.sliding(xs.length).contains(xs))
      || diff(IArray.from(xs), IArray.from(ys)).size.toDouble/(xs.length + ys.length) < 0.5

    val iterators: IArray[Iterator[List[Token]]] = IArray.from(samples.map(_.iterator))

    val lines: List[List[List[Token]]] = evolve(samples, similar).sequence.map: atom =>
      (0 until samples.length).to(List).map: lineNo =>
        if !atom.presence.contains(lineNo.z) then Nil else
          val line = iterators(lineNo).next()
          if line.all(_.text.s.forall(_.isWhitespace)) then List(Token(t" ", Accent.Unparsed))
          else line

    val radios =
      (0 until samples.length).map: sample =>
        Input.Radio
          (`class` = List(unsafely(Name[CssClass](t"s$sample"))), name = name, checked = sample == 0)
      . unless(samples.length <= 1)

    val code =
      lines.map: line =>
        evolve(line).sequence.map: atom =>
          val classes = atom.presence.to(List).map { step => unsafely(Name[CssClass](t"v${step.n0}")) }
          Code
            (`class` = classes ::: formattables.scala.classes(atom.value.accent).classes.to(List),
            style = t"width: ${atom.value.text.length}ch")(atom.value.text)
        :+ Code(`class` = List(unsafely(Name[CssClass](t"blank"))))
      . map: line =>
          Span.line(line*)

    if autoScale then Div(`class` = t"amok", style = t"font-size: ${fontSize}vw")(radios.or(Nil)*, Pre(code*))
    else Div.amok(radios.or(Nil)*, Pre(code*))

class AmokEmbedding(autoScale: Boolean)(using Tactic[TelError], Tactic[ParseError])
extends Formattable:
  def format(language: List[Text], content: Text): Html of Flow =
    val preamble = content.read[Tel].as[Preamble]

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
          tokens0:    List[Token],
          tokenStart: Int                      = -1,
          result:     List[Token | Note] = Nil)
    :     List[Token | Note] =

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
      ( markup: List[Token | Note],
        line:   List[Html of Phrasing]   = Nil,
        done:   List[Html of Phrasing]   = Nil )
    :   List[Html of Phrasing] =
      def render(token: Token | Note): Html of Phrasing = token match
        case Token(text, accent, _, _) =>
          formattables.scala.element(accent, text)

        case Note(tokens, style, caption) =>
          val captionSpan: List[Html of Phrasing] =
            caption.lay(Nil): text =>
              val textLines = text.cut(t"\n")
              val height = textLines.length*1.5
              val width = textLines.map(_.length).max + 1
              List(Span(`class` = t"caption", style = t"width:${width}ch;height:${height}em")(Span(text)))

          val content: List[Html of Phrasing] =
            captionSpan
            ++ tokens.reverse.map { token => formattables.scala.element(token.accent, token.text) }

          style match
            case Note.Style.Erroneous => Span.err(content*)
            case Note.Style.Caution   => Span.warn(content*)
            case Note.Style.Highlight => Span.hi(content*)
            case Note.Style.Param     => Span.param(content*)

      markup match
        case Nil                         => done.unwind(List(Span.line(line.reverse*)))
        case Token.Newline :: tail => lines(tail, Nil, Span.line(line.reverse*) :: done)
        case other :: tail               => lines(tail, render(other) :: line, done)

    def style(text: Text): Html of Flow =
      val ranges = errorRanges ++ cautionRanges ++ highlightRanges ++ paramRanges

      Pre
        ( lines
            ( selections
                ( ranges.compact.sortBy(_.start),
                  Scala.highlight(text, context).lines.to(List).flatMap(Token.Newline :: _) ) )
          . init
          . tail* )

    AmokEmbedding.format(preamble.frame.map(_.content), context, autoScale)
