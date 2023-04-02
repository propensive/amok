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

import rudiments.*
import cellulose.*

import language.dynamics

object Docs:
  opaque type Dictionary = Map[Text, Docs]

  object Dictionary:
    def apply(): Dictionary = Map()
    
    given (using CanThrow[IncompatibleTypeError]): Codec[Dictionary] with
      def schema = summon[Codec[Docs]].schema
      def serialize(value: Dictionary) = summon[Codec[List[Docs]]].serialize(value.values.to(List))
      
      def deserialize(value: List[Indexed]): Dictionary throws CodlReadError =
        summon[Codec[List[Docs]]].deserialize(value).map: docs =>
          docs.name -> docs
        .to(Map)
      
    extension (dict: Dictionary)
      def values: Iterable[Docs] = dict.values
      def empty: Boolean = dict.isEmpty
      def add(value: Docs): Dictionary = dict.get(value.name) match
        case None =>
          dict.updated(value.name, value)
        
        case Some(cdocs) => 
          val cdocs2: Docs = value.term.values.foldLeft(cdocs): (a, n) =>
            a.copy(term = a.term.add(n))

          val cdocs3: Docs = value.`type`.values.foldLeft(cdocs2): (a, n) =>
            a.copy(`type` = a.`type`.add(n))
            
          dict.updated(value.name, cdocs3)

import Docs.Dictionary

case class Docs(name: Text, icon: Text, summary: Maybe[Text] = Unset, doc: Maybe[Text] = Unset,
                    term: Dictionary = Dictionary(), `type`: Dictionary = Dictionary(),
                    value: Maybe[Int] = Unset):
  def addTerm(t: Docs): Docs throws IncompatibleTypeError = copy(term = term.add(t))
  def addType(t: Docs): Docs throws IncompatibleTypeError = copy(`type` = `type`.add(t))
  def empty: Boolean = term.empty && `type`.empty

