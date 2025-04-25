                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                               ╭───╮                              ┃
┃                                                               │   │                              ┃
┃                                                               │   │                              ┃
┃                        ╭─────────╮╭───╮╌────╮╌────╮╭─────────╮│   │ ╭───╮                        ┃
┃                        ╰─────╮   ││   ╭─╮   ╭─╮   ││   ╭─╮   ││   │╌╯   │                        ┃
┃                        ╭─────╯   ││   │ │   │ │   ││   │ │   ││        ╌╮                        ┃
┃                        │   ╭─╮   ││   │ │   │ │   ││   │ │   ││   ╭─╮   │                        ┃
┃                        │   ╰─╯   ││   │ │   │ │   ││   ╰─╯   ││   │ │   │                        ┃
┃                        ╰─────────╯╰───╯ ╰───╯ ╰───╯╰─────────╯╰───╯ ╰───╯                        ┃
┃                                                                                                  ┃
┃    Amok, version 0.1.0.                                                                          ┃
┃    © Copyright 2022-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://amok.propensive.com/                                                              ┃
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
import telekinesis.*

import strategies.throwUnsafely
import httpServers.stdlib
import asyncTermination.cancel
import logging.silent
import charEncoders.utf8
import supervisors.global

import html5.*

object Tests extends Suite(m"Amok Tests"):
  def run(): Unit =
    given Translator = HtmlTranslator(AmokEmbedding())

    val styles = t"""
@import url('https://fonts.googleapis.com/css2?family=Sono:wght,MONO@200..800,0..1&display=swap');
@import url('https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@100..800&display=swap');

pre code {
  font-family: Sono;
  font-variation-settings: "MONO" 1;
  font-weight: 300;
  font-size: 0.75em;
  line-height: 1;
}
.mono pre {
  font-family: monospace;
  font-weight: 100;
  font-size: 0.76em !important;
  line-height: 1.25em !important;
}

.amok {
  background-color: #023;
  margin: 0 -1em 0 -1em;
  &>pre {
    background-color: #023;
    color: #cb8;
    font-variation-settings: "MONO" 1;
    padding: 0.8em 1em;
    margin: 0;
    overflow-x: auto;
    scrollbar-color: #fff3 transparent;
    scrollbar-width: thin;
    &>.line {
      padding: 0.05em 0;
      display: block;
      height: 1.25em;
      line-height: 1.25em;
      &>.hi { --caption-color: 187, 187, 255; }
      &>.err { --caption-color: 255, 0, 68; }
      &>.warn { --caption-color: 255, 170, 0; }
      &>.param { font-style: italic; }
      & code {
        white-space: pre;
        border-bottom: solid 1px rgba(var(--caption-color), 0.5);
        background-color: rgba(var(--caption-color), 0.25);
        display: inline-block;
        margin: 0;
        padding: 0;
        vertical-align: middle; }
      &>span:hover {
        &>.caption {
          transition: height ease 0.2s 0.1s, width ease 0.2s;
          &>span {
            opacity: 1;
            transition: opacity ease 0.2s 0.2s; } } }
      &>span:not(:hover) {
        &>.caption {
          height: 0 !important;
          width: 0 !important;
          &>span {
            height: 0;
            transition: opacity ease 0.2s; } } }
      &>span {
        font-weight: 500;
        cursor: pointer;
        &>.caption {
          font-family: Sono;
          backdrop-filter: blur(2px);
          position: absolute;
          transition: height ease 0.2s, width ease 0.2s 0.1s;
          margin-top: 1.92em;
          font-weight: 350;
          font-size: 0.55em;
          color: white;
          padding: 0;
          line-height: 1.5em;
          background-color: rgba(var(--caption-color), 0.25);
          border-bottom: dotted 1px rgba(var(--caption-color), 0.5);
          &::after {
            background-color: rgb(var(--caption-color));
            content: '';
            position: absolute;
            bottom: -2.5px;
            right: -2.5px;
            width: 4px;
            height: 4px;
            border-radius: 50%; }
          &>span {
            opacity: 0;
            overflow: hidden;
            display: inline-block;
            margin: 0 0.5ch; } } } } }
  & input[type=radio] { display: none; }
  & .fore:checked ~ pre {
    & .one {
      opacity: 1;
      transition: height ease 1s 1s, width ease 1s 1s, opacity ease 1s 2s; }
    & .two {
      opacity: 0;
      transition: height ease 1s 1s, width ease 1s 1s, opacity ease 1s; }
    &>.two { height: 0; }
    &>.line>.two { width: 0 !important; } }
  & .aft:checked ~ pre {
    & .one {
      opacity: 0;
      transition: height ease 1s 1s, width ease 1s 1s, opacity ease 1s; }
    & .two {
      opacity: 1;
      transition: height ease 1s 1s, width ease 1s 1s, opacity ease 1s 2s; }
    &>.one { height: 0; }
    &>.line>.one { width: 0 !important; } }
  & .fore:checked + label::after {
    background-color: #fc3;
    color: black; }
  & .aft:checked + label::before {
    background-color: #fc3;
    color: black; }
  & .fore + label {
    width: 50%;
    text-align: right; }
  & label {
    cursor: pointer;
    font-size: 0.6em;
    color: #fffc;
    font-weight: 400;
    display: inline-block;
    text-transform: uppercase;
    padding: 0;
    margin: 1em 0 0 0;
    &::before, &::after {
      transition: background-color ease 0.3s, color ease 0.3s;
      display: inline-block;
      width: 1.8em;
      height: 1.7em;
      color: #fc2;
      border-radius: 50%;
      cursor: pointer;
      border: solid 0.15em #fc2;
      text-align: center;
      margin-left: 0.7em;
      margin-right: 0.7em; }
    &:hover::before {
      background-color: #fc3;
      color: black; }
    &:nth-of-type(1)::after { content: '1'; }
    &:nth-of-type(2)::before { content: '2'; } }
  & .error { color: #cc0033; }
  & .number { color: #cc3366; }
  & .modifier { color: #ff9966; }
  & .keyword { color: #ff6633; }
  & .ident { color: #ffcc99; }
  & .term { color: #ffcc33; }
  & .typed { color: #00cc99; }
  & .string { color: #99ddbb; }
  & .parens { color: #cc6699; }
  & .symbol { color: #cc3366; }
   & .unparsed { color: #2288aa; } } }
"""

    val markdown = md"""
## Heading 2

Hello _world_:
```amok
syntax scala
step
    object Foo:
      def bar: Unit = println("Hello world")
step
    object Baz:
      def foo: Unit = println("Hello world!")
step
    class Baz():
      def foo(): Unit = ()
##
object Foo
```
"""


    tcp"8080".serve:
      Http.Response:
        HtmlDoc(Html(Head(html5.Style(styles), Title(t"Example")), Body(markdown.html)))


    snooze(20.0*Second)

    // test(t"root package"):
    //   Path.Root.text
    // . assert(_ == t"_root_")

    // test(t"package name"):
    //   Path.Term(Path.Root, t"escritoire").text
    // . assert(_ == t"escritoire")

    // test(t"class name"):
    //   Path.Type(Path.Term(Path.Root, t"escritoire"), t"Column").text
    // . assert(_ == t"escritoire.Column")

    // test(t"class method name"):
    //   Path.Term(Path.Type(Path.Term(Path.Root, t"escritoire"), t"Column"), t"apply").text
    // . assert(_ == t"escritoire.Column#apply")

    // test(t"Read simple name"):
    //   Identifier(t"escritoire.Column")
    // . assert(_ == Identifier(Path.Term(Path.Root, t"escritoire"), t"Column"))

    // test(t"Read class method"):
    //   Identifier(t"escritoire.Column#width")
    // . assert(_ == Identifier(Path.Type(Path.Term(Path.Root, t"escritoire"), t"Column"), t"width"))

    // val examples =
    //   List
    //    (t"escritoire.Column#apply",
    //     t"escritoire.Column.width",
    //     t"Column#Cell#width",
    //     t"Column#Cell.apply")

    // for eg <- examples do test(t"Roundtrip test: $eg"):
    //   Identifier(eg).text
    // . assert(_ == eg)
