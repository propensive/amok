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

package rootpackage

object MyObject

class MyClass

object MyClassWithCompanion
class MyClassWithCompanion

val topLevelVal: Int = 0
var topLevelVar: Int = 0
def topLevelDef: Int = 0
inline def topLevelInlineDef: Int = 0
transparent inline def topLevelTransparentInlineDef: Int = 0

trait MyTrait

abstract class MyAbstractClass()

open class MyOpenClass()


package innerpackage:
  val innerpackageVal: Int = 0


object Nested:
  val myVal: Int = 0
  lazy val myLazyVal: Int = 0
  var myVar: Int = 0
  def myDef: Int = 0

  inline def myInlineDef: Int = 0

  transparent inline def myTransparentInlineDef: Int = 0

  erased val myErasedVal: Int = compiletime.erasedValue