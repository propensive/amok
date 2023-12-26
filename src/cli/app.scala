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
import rudiments.*
import exoskeleton.*, executives.completions, unhandledErrors.stackTrace, parameterInterpretation.posix
import eucalyptus.*
import anthology.*
import turbulence.*
import spectacular.*
import gossamer.*
import cellulose.*, codlPrinters.standard
import vacuous.*
import iridescence.*
import anticipation.*, fileApi.galileiApi
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

            execute:
              val file = params.File().or(abort(AmokError(msg"The file has not been specified")))
              
              val classpath: LocalClasspath = LocalClasspath:
                params.Classpath().or(abort(AmokError(msg"The classpath has not been specified"))).cut(t":").map: path =>
                  val path2 = safely(path.decodeAs[Path]).or(path.decodeAs[Unix.Link].inWorkingDirectory)
                  if path2.is[Directory] then ClasspathEntry.Directory(path2.show)
                  else ClasspathEntry.Jarfile(path2.show)

              val markdown = safely(file.decodeAs[Path]).or(file.decodeAs[Unix.Link].inWorkingDirectory).as[File].readAs[Text]
              
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

              def recur(codeSize: Int, todo: List[(Fragment, Text)]): Unit = todo match
                case Nil =>
                  ()
                
                case (fragment, code) :: rest =>
                  val errors2 = errors.filter { diagnostic => diagnostic.pos.end > codeSize && diagnostic.pos.start < codeSize+code.length }
                  
                  if errors2.length > 0 then
                    code.cut(t"\n").init.map { line => Out.println(e"${Bg(colors.Crimson)}( ) $line") }
                    Out.println(e"${colors.Crimson}(│)")
                    errors2.zipWithIndex.foreach: (diagnostic, index) =>
                      diagnostic.message.tt.trim.cut(t"\n").foreach: line =>
                        Out.println(e"${colors.Crimson}(│)$Italic(${colors.Silver}( ${line}))")
                      Out.println(e"${colors.Crimson}(${if index < errors2.length - 1 then t"├" else t"└"})")
                    
                    Out.println(t"")

                  recur(codeSize+code.length, rest)

              recur(0, fragments.to(List))

              if errors.isEmpty then ExitStatus.Ok else ExitStatus.Fail(1)
          case params.Shutdown() =>
            execute:
              service.shutdown()
              ExitStatus.Ok

          case _ =>
            execute:
              Out.println(t"Unknown command")
              ExitStatus.Fail(1)
