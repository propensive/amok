/*
    , version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import galilei.*, filesystems.unix
import anticipation.*, fileApi.galileiApi

import unsafeExceptions.canThrowAny

object Tests extends Suite(t"Amok Tests"):
  def run(): Unit =
    val files = Unix.parse(t"/home/propensive/.cache/irk/cls/amok/entities").directory(Expect).descendants.filter(_.name.ends(t".tasty")).files
    val docs: Docs = Amok.inspect(files)
    
    test(t"Read object"):
      docs.rootpackage.MyObject
    .assert(_.entity == Entity.Module)
    
    test(t"Read class"):
      docs.rootpackage.MyClass()
    .assert(_.entity == Entity.Class(`abstract` = false))

    test(t"Read abstract class"):
      docs.rootpackage.MyAbstractClass()
    .assert(_.entity == Entity.Class(`abstract` = true))
    
    test(t"Read trait"):
      docs.rootpackage.MyTrait()
    .assert(_.entity == Entity.Trait)
