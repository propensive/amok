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

import rudiments.*
import gossamer.*
import probably.*

import unsafeExceptions.canThrowAny

object Tests extends Suite(t"Amok Tests"):
  def run(): Unit =
    test(t"root package"):
      Path.Root.text
    .assert(_ == t"_root_")
    
    test(t"package name"):
      Path.Term(Path.Root, t"escritoire").text
    .assert(_ == t"escritoire")
    
    test(t"class name"):
      Path.Type(Path.Term(Path.Root, t"escritoire"), t"Column").text
    .assert(_ == t"escritoire.Column")
    
    test(t"class method name"):
      Path.Term(Path.Type(Path.Term(Path.Root, t"escritoire"), t"Column"), t"apply").text
    .assert(_ == t"escritoire.Column#apply")

    test(t"Read simple name"):
      Name(t"escritoire.Column")
    .assert(_ == Name(Path.Term(Path.Root, t"escritoire"), t"Column"))
    
    test(t"Read class method"):
      Name(t"escritoire.Column#width")
    .assert(_ == Name(Path.Type(Path.Term(Path.Root, t"escritoire"), t"Column"), t"width"))

    val examples = List(t"escritoire.Column#apply", t"escritoire.Column.width", t"Column#Cell#width",
        t"Column#Cell.apply")
    
    for eg <- examples do test(t"Roundtrip test: $eg"):
      Name(eg).text
    .assert(_ == eg)