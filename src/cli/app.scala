/*
    Amok, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package amok

import escapade.*
import parasite.*
import spectral.*
import aviation.*
import rudiments.*
import surveillance.*
import exoskeleton.*, executives.completions, unhandledErrors.stackTrace, parameterInterpretation.posix
import eucalyptus.*
import anthology.*
import profanity.*
import quantitative.*
import turbulence.*
import spectacular.*
import gossamer.*
import cellulose.*, codlPrinters.standard
import vacuous.*
import iridescence.*
import anticipation.*, fileApi.galileiApi, timeApi.aviationApi
import fulminate.*
import perforate.*
import punctuation.*
import hellenism.*
import galilei.*, filesystemOptions.{doNotCreateNonexistent, dereferenceSymlinks}
import serpentine.*, hierarchies.unix
import hieroglyph.*, charDecoders.utf8, badEncodingHandlers.strict
import ambience.*, environments.jvm, homeDirectories.default, systemProperties.jvm

given (using Cli): WorkingDirectory = workingDirectories.daemonClient 

case class AmokError(details: Message) extends Error(details)
case class Fragment(id: Text, language: Language, follows: Optional[Text] = Unset)
case class Language(compiler: Text, version: Text)

@main
def main(): Unit =
  unsafely:
    supervise:
      given Log[Output] = logging.silent[Output]
      
      object params:
        val Classpath = Flag[Text](t"classpath", false, List('c'), t"specify the classpath")
        val File = Flag[Text](t"file", false, List('f'), t"specify a file to check")
        val Watch = Switch(t"watch", false, List('w'), t"watch for changes")
        val Install = Subcommand(t"install", t"install the application")
        val Check = Subcommand(t"check", t"check a markdown file")
        val Shutdown = Subcommand(t"shutdown", t"stop Amok running as a background process")

      daemon:
        safely(arguments.head) match
          case params.Install() =>
            execute:
              Out.println(Installer.install().communicate)
              Out.println(TabCompletions.install(force = true).communicate)
              ExitStatus.Ok
          
          case params.Check() =>
            params.Classpath()
            params.File()
            params.Watch()

            execute:
              val file = params.File().or(abort(AmokError(msg"The file has not been specified")))
              
              val classpath: LocalClasspath = LocalClasspath:
                params.Classpath().or(abort(AmokError(msg"The classpath has not been specified"))).cut(t":").map: path =>
                  val path2 = safely(path.decodeAs[Path]).or(path.decodeAs[Unix.Link].inWorkingDirectory)
                  if path2.is[Directory] then ClasspathEntry.Directory(path2.show)
                  else ClasspathEntry.Jarfile(path2.show)

              val markdownFile = safely(file.decodeAs[Path]).or(file.decodeAs[Unix.Link].inWorkingDirectory).as[File]
              
              def recompile(): Unit =
                val markdown = markdownFile.readAs[Text]
                
                val fragments: Seq[(Fragment, Text)] =
                  Markdown.parse(markdown).nodes.collect:
                    case Markdown.Ast.Block.FencedCode(t"scala", meta, code) =>
                      val fragment = Fragment(t"id", Language(t"scala", t"3.3"))
                      fragment -> code

                    case Markdown.Ast.Block.FencedCode(t"amok", meta, code) =>
                      val codl: CodlDoc = Codl.parse(code)
                      val fragment = safely(codl.as[Fragment]).or(Fragment(t"id", Language(t"unknown", t"0.0")))
                      fragment -> codl.body.foldLeft(t"")(_ + _.show)

                val allCode: Text = fragments.map(_(1)).join
                val errors = Scalac(Map(t"fragments" -> allCode), classpath, workingDirectory, List())()

                def assign(codeSize: Int, todo: List[(Fragment, Text)]): Unit = todo match
                  case Nil =>
                    ()
                  
                  case (fragment, code) :: more =>
                    val errors2 = errors.filter { diagnostic => diagnostic.pos.end > codeSize && diagnostic.pos.start < codeSize+code.length }
                    
                    if errors2.length > 0 then
                      code.cut(t"\n").init.map { line => Out.println(e"${Bg(colors.Crimson)}( ) $line") }
                      Out.println(e"${colors.Crimson}(│)")
                      errors2.zipWithIndex.foreach: (diagnostic, index) =>
                        diagnostic.message.tt.trim.cut(t"\n").foreach: line =>
                          Out.println(e"${colors.Crimson}(│)$Italic(${colors.Silver}( ${line}))")
                        Out.println(e"${colors.Crimson}(${if index < errors2.length - 1 then t"├" else t"└"})")
                      
                      Out.println(t"")

                    assign(codeSize+code.length, more)
                
                assign(0, fragments.to(List))

                val errorCount: Output = errors.length match
                  case 0 => e"no errors"
                  case 1 => e"$Bold(one) error"
                  case 2 => e"$Bold(two) errors"
                  case 3 => e"$Bold(three) errors"
                  case 4 => e"$Bold(four) errors"
                  case n => e"$Bold($n) errors"

                Out.println(e"$Italic(Checked ${fragments.length} fragments, $errorCount)")
              
              terminal:
                def loop(stream: LazyList[TerminalEvent | List[WatchEvent]]): Unit =
                  recompile()
                  Out.println(e"Waiting for changes... $Italic[(press $Bold[q] to exit)]")
                  stream match
                    case Keypress.CharKey('q') #:: _ => ()
                    case event #:: more =>
                      event match
                        case event: List[WatchEvent] => Out.println(event.debug)
                        case event: TerminalEvent    => Out.println(event.debug)
                    
                      loop(more)
                    case _ => ()

                loop:
                  if params.Watch().absent then LazyList()
                  else markdownFile.path.parent.vouch(using Unsafe).as[Directory].watch().stream.cluster(0.1*Second).multiplexWith(terminal.events)

              ExitStatus.Ok
          
          case params.Shutdown() =>
            execute:
              service.shutdown()
              ExitStatus.Ok

          case _ =>
            execute:
              Out.println(t"Unknown command")
              ExitStatus.Fail(1)
