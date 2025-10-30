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

import errorDiagnostics.stackTraces
import codlPrinters.standard

object Folio:
  given Folio is Tabulable[Text] = () =>
    Table[Folio, Text](Column(t"", TextAlignment.Left, Unset, columnar.Prose):
      case folio: JvmFolio => t"jvm"
      case folio           => t"content")

  def load(mountpoint: Mountpoint, file: Path on Linux)(using Stdio, Terminal): Folio raises LoadError =
    val folio: Optional[Folio] = Server(mountpoint)
    if file.name.ends(t".jar") then
      Out.println(m"Loading JAR file ${file.name}")

      folio.or(JvmFolio(mountpoint, file)).match
        case folio: JvmFolio => folio
        case folio           =>
          Out.println(m"Replacing pre-existing folio")
          JvmFolio(mountpoint, file)

      . tap(_.model.load(file))

    else if file.name.ends(t".amok") then
      val amox = safely(Amox.read(file))

      amox.let: amox =>
        folio.match
          case Unset           => JvmFolio(mountpoint, file)
          case folio: JvmFolio => folio
          case folio           =>
            Out.println(m"Replacing pre-existing folio")
            JvmFolio(mountpoint, file)

        // . tap(_.model.overlay(amox))
      . or:
          mitigate:
            case error@IoError(path, _, _)   => LoadError(file, error)
            case error@StreamError(memory)   => LoadError(file, error)
            case error@ParseError(_, _, _)   => LoadError(file, error)
            case error@CodlError(_)          => LoadError(file, error)
            case error@NumberError(_, _)     => LoadError(file, error)
            case error@MarkdownError(_)      => LoadError(file, error)

          . within:
              val text = file.open(_.read[Text])
              val stripped = safely(text.cut(t"\n").dropWhile(_ != "##").tail).or(Nil).join(t"\n")
              val doc = text.read[CodlDoc of AmokDoc].materialize

              given Model()

              val sections = stripped.read[Md].broken.map(_.html).zipWithIndex.map: (content, index) =>
                html5.Section(id = DomId(t"slide${index + 1}"))(content)

              SlidesFolio(mountpoint, doc, file, sections)

    else abort(LoadError(file, FiletypeError()))

trait Folio(val base: Mountpoint, val kind: Text, val source: Path on Linux):
  def subpath(using Http.Request, Stdio): Path on Www under %.type = request.path.shift(base.endpoint.depth)

  def handle(using Http.Request, Stdio): Http.Response
