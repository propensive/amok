package amok

import scala.quoted.*

import soundness.{is as _, Node as _, *}

export executives.completions
export unhandledErrors.stackTrace
export parameterInterpretation.posix
export threadModels.platform
export workingDirectories.daemonClient
export logging.silent
export filesystemOptions.dereferenceSymlinks.{enabled as dereferencingEnabled}
export filesystemOptions.readAccess.{enabled as readAccessEnabled}
export filesystemOptions.writeAccess.{disabled as writeAccessDisabled}
export filesystemOptions.createNonexistent.{disabled as creationDisabled}
export alphabets.base64.standard
export treeStyles.default
export httpServers.stdlibPublic
export asyncTermination.cancel
export supervisors.global
export charEncoders.utf8
export charDecoders.utf8
export textSanitizers.skip
export classloaders.threadContext

given Imports(Set
       (Index.decode(t"scala.Predef"),
        Index.decode(t"prepositional"),
        Index.decode(t"java.lang")))
