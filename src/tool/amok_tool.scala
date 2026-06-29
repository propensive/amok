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

import soundness.{Node as _, *}
import errorDiagnostics.stackTracesDiagnostics
import textMetrics.uniformMetric
import tableStyles.horizontalTableStyle
import columnAttenuation.ignoreAttenuation

val About   = Subcommand("about",  e"find out about Amok")
val Load    = Subcommand("load",   e"load definitions from a .jar or .amok file")
val Boot    = Subcommand("boot",   e"start Amok using the .amok file in the current directory")
val Clear   = Subcommand("clear",  e"clear definitions from a JAR file")
val Quit    = Subcommand("quit",   e"shutdown Amok")
val Serve   = Subcommand("serve",  e"serve the documentation on a local HTTP server")
val Search  = Subcommand("search", e"search for an entity by name")
val Folios  = Subcommand("list",   e"list all deployed mountpoints")

val MountpointArg =
  Flag[Mountpoint]
   ("mountpoint", false, List('m'), "the URL at which to serve the folio, e.g. /tutorials")


object External:
  def unapply(argument: Argument)(using WorkingDirectory, Cli): Option[(Path on Local) | HttpUrl] =
    if argument().starts("https:") || argument().starts("http:")
    then safely(Some(argument().decode[HttpUrl])).or(None)
    else Pathname.unapply(argument)

@main
def application(): Unit = cli:
  // Try to install tab-completions only once
  Completions.ensure()

  arguments match
    case About() :: _ => execute:
      recover:
        case SerializationError(_, _) => panic(m"Failed to deserialize")

      . protect:
          Out.println()
          t"""H4sIADMTXWgAA51RwQ3AIAj8OwWjNr77lDFqnYlJGoO2ojaChhjEu+NQAO0ivCgcJTA6NREoeN63OKLvGBh7Z4
              RnhxgroFCtDobJCdMEX7DMYgZX23yO+KrBZ694/89e71p/pqx8Zu7iFl6smdR51ZPd95oz+4XunQTCet5RdA9q
              ts7STgMAAA=="""
          . erase(' ', '\n')
          . deserialize[Base64]
          . gunzip
          . utf8
          . cut(t"\n")
          . each(Out.println(_))

          given Decimalizer(significantFigures = 4, exponentThreshold = Unset)
          val memory = Heap.used/1.mib
          val build = cp"/build.id".read[Text].trim
          val tagline = "a documentation engine for Scala"
          Out.println(e"$Bold(Amok) prerelease version 0.1.0, build $build: $Italic($tagline)")
          Out.println(e"© Copyright 2025 Propensive OÜ")
          Out.println()
          Out.println(e"Memory usage: $memory MiB")
          Out.println()

      Exit.Ok

    case Folios() :: _ => execute:
      val base = workingDirectory[Path on Local]
      val table =
        Scaffold[Folio, Text]
         (Column(t"Mountpoint")((folio: Folio) => folio.base.show),
          Column(t"Source")((folio: Folio) => folio.source.toward(base).encode),
          Column(t"Type")((folio: Folio) => folio.kind))

      recover:
        case TerminalError() =>
          Out.println(table.tabulate(Server.folios).grid(100).render.join(t"\n"))

      . protect:
          interactive:
            Out.println(table.tabulate(Server.folios).grid(terminal.knownColumns).render.join(t"\n"))

      Exit.Ok

    case Load() :: External(external) :: _ =>
      safely(MountpointArg())
      execute:
        try
          recover:
            case error@LoadError(_, _) =>
              Out.println(error.message)
              Exit.Fail(1)
          . protect:
              val mountpoint = MountpointArg().or(Mountpoint())
              val file = external match
                case url: HttpUrl =>
                  import internetAccess.online
                  val temporaryFile: Path on Local = unsafely(temporaryDirectory[Path on Local] / url.location.decode[Path on Www].name)
                  Out.println(m"Downloading $url")
                  unsafely:
                    temporaryFile.open: handle =>
                      url.fetch().writeTo(handle)
                  Out.println(m"Deploying $url to $mountpoint".teletype)
                  temporaryFile

                case file: (Path on Local) =>
                  Out.println(m"Deploying $file to $mountpoint".teletype)
                  file

              safely:
                interactive:
                  Server.register(Folio.load(mountpoint, file))
              Exit.Ok

        catch case error: Throwable =>
          Out.println(error.stackTrace.teletype)
          Out.println(m"Error: ${error.toString}") yet Exit.Ok

    case Quit() :: _ => execute(service.shutdown() yet Exit.Ok)

    case Search() :: term :: Nil =>
      safely(MountpointArg())

      execute:
        given mountpoint: Mountpoint = MountpointArg().or(Mountpoint())

        Server(mountpoint) match
          case folio: JvmFolio =>
            given Model = folio.model
            given Imports = Imports(Set(), Set())
            folio.model.search(term()).each: member =>
              folio.model.lookup(member).let: node =>
                val info = node.info.lay(e"") { info => e": $Italic(${info.toString.tt})" }
                Out.println(e"${member.parent.lay(t"")(_.render)}${member.symbol}$Bold(${member.name})$info")

                node.declarations.each: declaration =>
                  Out.println(declaration.syntax(false).show)
                Out.println()
            Exit.Ok
          case _ =>
            Out.println(m"This mountpoint is not searchable.") yet Exit.Fail(1)

    case Serve() :: _ => execute:
      recover:
        case ServerError(port) =>
          Out.println(m"Can't start a server on port $port") yet Exit.Fail(1)

        case ClasspathError(path) =>
          panic(m"Expected to find $path on the classpath")

        case TerminalError() =>
          panic(m"Could not start the terminal")

      . protect:
          interactive:
            Out.println(e"Listening on $Italic(http://localhost:8080)")
            Out.println(e"Type Ctrl+C to exit")
            tcp"8080".serve:
              def notFound = Page.simple(Mountpoint(), t"There is no page at ${request.location}")
              try Server.at(request.location).lay(Http.Response(NotFound(notFound)))(_.handle)
              catch case throwable: Throwable =>
                Out.println(throwable.stackTrace.teletype)
                Http.Response(t"Error: ${throwable.toString}")

            terminal.events.stream.takeWhile(_ != Keypress.Ctrl('C')).strict

          Exit.Ok

    case command :: _ =>
      execute(Out.println(m"Unknown command: ${command()}") yet Exit.Fail(1))

    case Nil =>
      execute(Out.println(m"Please specify a command") yet Exit.Fail(1))
