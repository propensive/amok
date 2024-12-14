/*
    Amok, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import contingency.*
import digression.*
import galilei.*
import hellenism.*, classloaders.threadContext
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8, textSanitizers.strict
import rudiments.*
import serpentine.*
import turbulence.*

object data:
  def font(name: Name[Classpath])(using Tactic[ClasspathError]): Bytes =
    (Classpath / n"amok" / n"fonts" / name)().read[Bytes]

  def image(name: Name[ClasspathRef.Forbidden])
     (using Tactic[ClasspathError], Tactic[CharDecodeError])
          : Text =
    (Classpath / n"amok" / n"images" / name)().read[Text]
