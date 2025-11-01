                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.45.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
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

import soundness.*

import autopsies.contrastExpectations

object Tests extends Suite(m"Amok Tests"):
  def run(): Unit =
    suite(m"Criteria tests"):
      import Criterion.*
      test(m"+List"):
        t"+List".decode[Criteria]
      . assert(_ == Criteria(ReturnType("List")))

      test(m"+List[Int]"):
        t"+List[Int]".decode[Criteria]
      . assert(_ == Criteria(ReturnType("List[Int]")))

      test(m"-Either[Int, String]"):
        t"-Either[Int, String]".decode[Criteria]
      . assert(_ == Criteria(Parameter("Either[Int, String]")))

      test(m"+List[Int] -String"):
        t"+List[Int] -String".decode[Criteria]
      . assert(_ == Criteria(ReturnType("List[Int]"), Parameter("String")))

      test(m"+(Int, Double, String) -String"):
        t"+(Int, Double, String) -String".decode[Criteria]
      . assert(_ == Criteria(ReturnType("(Int, Double, String)"), Parameter("String")))

      test(m"+\"hello world\" -\"foo bar\""):
        t"""+"hello world" -"foo bar"""".decode[Criteria]
      . assert(_ == Criteria(ReturnType(""""hello world""""), Parameter(""""foo bar"""")))

      test(m"testing -String +Int"):
        t"testing +String -Int".decode[Criteria]
      . assert(_ == Criteria(Name("testing"), ReturnType("String"), Parameter("Int")))

      test(m"testing +String -Int"):
        t"testing +String -Int".decode[Criteria]
      . assert(_ == Criteria(Name("testing"), ReturnType("String"), Parameter("Int")))

      test(m":testing +String -Int"):
        t":testing +String -Int".decode[Criteria]
      . assert(_ == Criteria(Type, Name("testing"), ReturnType("String"), Parameter("Int")))

      test(m".testing +String -Int"):
        t".testing +String -Int".decode[Criteria]
      . assert(_ == Criteria(Term, Name("testing"), ReturnType("String"), Parameter("Int")))

      test(m"!mypackage .testing +String -Int"):
        t"!mypackage .testing +String -Int".decode[Criteria]
      . assert(_ == Criteria(Package("mypackage"), Term, Name("testing"), ReturnType("String"), Parameter("Int")))

      test(m"!mypackage .testing +String -Int"):
        t"!mypackage .testing +String -Int".decode[Criteria]
      . assert(_ == Criteria(Package("mypackage"), Term, Name("testing"), ReturnType("String"), Parameter("Int")))

      test(m"!mypackage . testing +String -Int"):
        t"!mypackage . testing +String -Int".decode[Criteria]
      . assert(_ == Criteria(Package("mypackage"), Term, Name("testing"), ReturnType("String"), Parameter("Int")))

      test(m"!mypackage : Testing +String -Int"):
        t"!mypackage : Testing +String -Int".decode[Criteria]
      . assert(_ == Criteria(Package("mypackage"), Type, Name("Testing"), ReturnType("String"), Parameter("Int")))

      test(m"!my.package.name :Testing +String -Int"):
        t"!my.package.name :Testing +String -Int".decode[Criteria]
      . assert(_ == Criteria(Package("my.package.name"), Type, Name("Testing"), ReturnType("String"), Parameter("Int")))

      test(m"!my.package.name :Testing +String -Int "):
        t"!my.package.name :Testing +String -Int ".decode[Criteria]
      . assert(_ == Criteria(Package("my.package.name"), Type, Name("Testing"), ReturnType("String"), Parameter("Int")))

      test(m"!my.package.name :Testing +List[String => Int] -Int "):
        t"!my.package.name :Testing +List[String => Int] -Int ".decode[Criteria]
      . assert(_ == Criteria(Package("my.package.name"), Type, Name("Testing"), ReturnType("List[String => Int]"), Parameter("Int")))

      test(m"!my.package.name :Testing +List[String => Int] -(Int ?=> Int)"):
        t"!my.package.name :Testing +List[String => Int] -(Int ?=> Int) ".decode[Criteria]
      . assert(_ == Criteria(Package("my.package.name"), Type, Name("Testing"), ReturnType("List[String => Int]"), Parameter("(Int ?=> Int)")))
