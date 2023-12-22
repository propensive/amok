package amok

import escapade.*
import parasite.*
import spectral.*
import rudiments.*
import exoskeleton.*, executives.completions, unhandledErrors.stackTrace, parameterInterpretation.posix
import eucalyptus.*
import turbulence.*
import gossamer.*
import anticipation.*
import perforate.*

@main
def main(): Unit =
  unsafely:
    supervise:
      given Log[Output] = logging.silent[Output]

      daemon:
        execute:
          Out.println(t"Hello world")
          ExitStatus.Ok

