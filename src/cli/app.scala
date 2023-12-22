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
import turbulence.*
import gossamer.*
import vacuous.*
import anticipation.*
import fulminate.*
import perforate.*
import ambience.*, environments.jvm, homeDirectories.default

given (using Cli): WorkingDirectory = workingDirectories.daemonClient 

@main
def main(): Unit =
  unsafely:
    supervise:
      given Log[Output] = logging.silent[Output]
      
      val Classpath = Flag[Text](t"classpath", false, List('c'), t"Specify the classpath")

      daemon:
        val classpath = Classpath()
        execute:
          Out.println(Installer.install().communicate)
          Out.println(TabCompletions.install(force = true).communicate)
          Out.println(t"Hello world!")
          Out.println(t"The classpath is ${classpath.or(t"unknown")}")
          ExitStatus.Ok

