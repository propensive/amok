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

import scala.tasty.*, inspector.*
import scala.quoted.*

import soundness.{is as _, Node as _, *}
import errorDiagnostics.stackTraces
import textMetrics.uniform
import tableStyles.horizontal
import columnAttenuation.ignore
import enumIdentification.kebabCase

given Tactic[CodlError] => Tactic[CodlReadError] => Translator =
  HtmlTranslator(AmokEmbedding(false), ScalaEmbedding)

val About   = Subcommand("about",   e"find out about Amok")
val Load    = Subcommand("load",    e"load definitions from a .jar or .amok file")
val Present = Subcommand("present", e"give a presentation from a .amok file")
val Boot    = Subcommand("boot",    e"start Amok using the .amok file in the current directory")
val Clear   = Subcommand("clear",   e"clear definitions from a JAR file")
val Quit    = Subcommand("quit",    e"shutdown Amok")
val Serve   = Subcommand("serve",   e"serve the documentation on a local HTTP server")
val Folios  = Subcommand("list",    e"list all deployed endpoints")

val FolioArg = Flag("folio", false, List('f'), "the URL path at which to serve the folio, e.g. /tutorials")

enum Number:
  case One, Two, Three, Four, Five, Six, Seven

def load(endpoint: Text, file: Path on Linux)(using Stdio): Folio raises LoadError =
  val folio: Optional[Folio] = Server(endpoint)
  if file.name.ends(t".jar") then
    Out.println(m"Loading JAR file ${file.name}")
    folio.or(JvmFolio(endpoint)).match
      case folio: JvmFolio => folio
      case folio           =>
        Out.println(m"Replacing pre-existing folio at ${folio.path}")
        JvmFolio(endpoint)

    . tap(_.model.load(file))

  else if file.name.ends(t".amok") then
    val amox = Amox.read(file)

    folio.match
      case Unset           => JvmFolio(endpoint)
      case folio: JvmFolio => folio
      case folio           =>
        Out.println(m"Replacing pre-existing folio${folio.let(t" at "+_.path).or(t"")}")
        JvmFolio(endpoint)

    . tap(_.model.overlay(amox))

  else abort(LoadError(file, FiletypeError()))

@main
def application(): Unit = cli:
  // Try to install tab-completions only once
  Completions.ensure()

  arguments match
    case About() :: _ => execute:
      recover:
        case SerializationError(_, _) => panic(m"Failed to deserialize")

      . within:
          Out.println()
          t"""H4sIADMTXWgAA51RwQ3AIAj8OwWjNr77lDFqnYlJGoO2ojaChhjEu+NQAO0ivCgcJTA6NREoeN63OKLvGBh7Z4
              RnhxgroFCtDobJCdMEX7DMYgZX23yO+KrBZ694/89e71p/pqx8Zu7iFl6smdR51ZPd95oz+4XunQTCet5RdA9q
              ts7STgMAAA=="""
          . erase(' ', '\n')
          . deserialize[Base64]
          . gunzip
          . utf8
          . cut(t"\n")
          . each: line =>
              Out.println(line)

          given Decimalizer(significantFigures = 4, exponentThreshold = Unset)
          val memory = Heap.used/1.mib
          val build = unsafely((Classpath / "build.id").read[Text].trim)

          Out.println(e"$Bold(Amok) prerelease version, build $build: $Italic(a documentation engine for Scala)")
          Out.println(e"© Copyright 2025 Propensive OÜ")
          Out.println()
          Out.println(e"Memory usage: $memory MiB")
          Out.println()

      Exit.Ok

    case Folios() :: _ =>
      val number = Flag("number", false, List('n'), "just a number")
      safely(number[Number]())

      execute:
        Out.println(safely(number[Number]()).let(_.show).or(t"-"))
        Out.println(Server.mappings.to(List).table)
        Exit.Ok

    case Load() :: Pathname(file) :: _ =>
      safely(FolioArg[Number]())
      execute:
        recover:
          case error@LoadError(_, _) =>
            Out.println(error.message)
            Exit.Fail(1)
        . within:
            FolioArg[Text]().let: endpoint =>
              Server.register(endpoint, load(endpoint, file))
              Exit.Ok
            . or:
                Exit.Fail(1)

    case Present() :: Pathname(file) :: _ =>
      safely(FolioArg[Number]())

      execute:
        Out.println(m"Loading ${file.name}...")
        recover:
          case IoError(path, _, _) =>
            Out.println(m"Failed to open file ${path.name}") yet Exit.Fail(1)

          case StreamError(memory) =>
            Out.println(m"Failed to read file after $memory") yet Exit.Fail(1)

          case error@CodlReadError(_) =>
            Out.println(m"CoDL read error (${error.toString.tt}) in ${file.name}") yet Exit.Fail(5)

          case error@CodlError(_, _, _, _) =>
            Out.println(m"CoDL error (${error.toString.tt}) in ${file.name}") yet Exit.Fail(5)

          case NumberError(_, _) =>
            Out.println(m"Not a number") yet Exit.Fail(1)

          case MarkdownError(_) =>
            Out.println(m"Markdown error") yet Exit.Fail(1)

        . within:
            val text = file.open(_.read[Text])
            val stripped = text.cut(t"\n").dropWhile(_ != "##").tail.join(t"\n")
            val doc = Codl.read[AmokDoc](text)

            val sections = Markdown.parse(stripped).broken.map(_.html).zipWithIndex.map: (content, index) =>
              html5.Section(id = DomId(t"slide${index + 1}"))(content)

            import html5.*

            val folio = new Folio(t"/"):
              def handle(request: Http.Request): Http.Response = Http.Response:
                HtmlDoc:
                  Html
                   (Head
                     (html5.Link.Stylesheet(href = t"/code.css"),
                      html5.Script(src = t"/navigate.js"),
                      Title(doc.title)),
                    Body(Div.visible(id = id"overlay"), Main(sections*)))

            Server.register(t"/", folio)

            Exit.Ok

    case Quit() :: _ => execute(service.shutdown() yet Exit.Ok)

    case Serve() :: _ => execute:
      recover:
        case ServerError(port) =>
          Out.println(m"Can't start a server on port $port") yet Exit.Fail(1)

        case ClasspathError(path) =>
          panic(m"Expected to find $path on the classpath")

      . within:
          tcp"8080".serve:
            Http.Response:
              Server.mappings.keySet.find(request.location.starts(_)).map(_.show).optional.or(t"-")

          Out.println(e"Listening on $Bold(http://localhost:8080)")
          Exit.Ok

    case command :: _ =>
      execute(Out.println(m"Unknown command: ${command()}") yet Exit.Fail(1))

    case Nil =>
      execute(Out.println(m"Please specify a command") yet Exit.Fail(1))
