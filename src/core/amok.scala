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

import Docs.Dictionary

import eucalyptus.*
import anticipation.*
import gossamer.*
import rudiments.{Cursor as _, is as _, *}
import scala.quoted.*
import scala.tasty.inspector.*
import scala.tasty.*
import scala.compiletime.*

import unsafeExceptions.canThrowAny

given Realm = Realm(t"amok")

object Scope:
  given scope: Scope = Scope(Set(
    List(t"_root_", t"scala"),
    List(t"_root_", t"scala", t"caps")
  ))

case class Scope(packages: Set[List[Text]]):
  def prefix(path: List[Text]): Text =
    if packages.contains(path) then t"" else t"${path.join(t".")}."

case class Info(name: Text, modifiers: List[Text], signature: Text)

object Amok:
  def inspect[FileType: GenericFileReader](tastyFiles: Seq[FileType]): Docs =
    case class DocInspector() extends Inspector:
      private var rootDocs: Docs = Docs(t"_root_", t"", Unset, Unset, Dictionary(), Dictionary(), Unset)

      def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
        import quotes.reflect.*
        import Flags.*
        
        val retainsSym = TypeRepr.of[annotation.retains].typeSymbol
        
        def pname(term: Term, xs: List[Text] = Nil): List[Text] = term match
          case Ident(base)        => base.show :: xs
          case Select(parent, id) => pname(parent, id.show :: xs)
          

        def info(valDef: quotes.reflect.ValDef): Unit =
          val ValDef(name, returnType, _) = valDef
          val sym = valDef.symbol
          val flags = sym.flags
        
        def showType(using scope: Scope)(repr: quotes.reflect.TypeRepr, parens: Boolean = false): Text =
          repr.asMatchable match
            case AppliedType(base, args)           =>
              val argTypes = args.map(showType(_)).join(t", ")
              if defn.isTupleClass(base.typeSymbol) then t"($argTypes)"
              else t"${showType(base)}[$argTypes]"

            case ConstantType(IntConstant(int))    => int.show
            case ConstantType(StringConstant(str)) => t"\"$str\""
            case TypeRef(prefix, ref)              => ref.show
            case AnnotatedType(tpe, anns)          => t"${captures(anns)}${showType(tpe)}"
            case OrType(left, right)               => t"${showType(left, true)} | ${showType(right, true)}"
            case AndType(left, right)              => t"${showType(left, true)} & ${showType(right, true)}"
            case TermRef(prefix, name)             => t"$name.type"
            case TypeLambda(from, to, tpe)         => t"[${from.mkString(", ")}] =>> ${showType(tpe, true)}" // FIXME show bounds
            case TypeBounds(lb, ub)                => t"? >: ${showType(lb, true)} <: ${showType(ub, true)}}" // FIXME
            
            case ref: dotty.tools.dotc.core.Types.TypeParamRef =>
              ref.binder match { case TypeLambda(params, _, _) => params(ref.paramNum).show }
        
            case other =>
              println("Unmatched: "+other)
              t"???"
        
        def captures(using scope: Scope)(term: quotes.reflect.Term): Text =
          term match
            case Apply(Select(New(focus), _), List(Typed(Repeated(values, _), _))) if focus.tpe.typeSymbol == retainsSym =>
              values.map:
                case Select(prefix, name) => t"${scope.prefix(pname(prefix))}${name}"
                case Ident(name)          => t"${name}"
              .join(t"{", t", ", t"} ")
            case other => t""

        def icon(entityType: Icons.Entity, flags: Flags): Icons.Icon =
          import Icons.*
          var entity: Entity = entityType
          var qualifiers: Set[Qualifier] = Set()
          if entity != Entity.Package then
            if flags.is(Flags.Mutable) then entity = Entity.Var
            else if flags.is(Flags.Given) then entity = Entity.Given
            else if flags.is(Flags.Enum) then entity = if flags.is(Flags.Case) then Entity.Case else Entity.Enum
            else if flags.is(Flags.Trait) then entity = Entity.Trait
            else if flags.is(Flags.Module) then entity = Entity.Object
          //if flags.is(Flags.Final) then qualifiers += Qualifier.Final
          
          if flags.is(Flags.Lazy) then qualifiers += Qualifier.Lazy
          if flags.is(Flags.Open) then qualifiers += Qualifier.OpaqueOrOpen
          if flags.is(Flags.ExtensionMethod) then qualifiers += Qualifier.Extension
          //if flags.is(Flags.Accessor) then qualifiers += Qualifier.Param
          
          if flags.is(Flags.Opaque) then qualifiers += Qualifier.OpaqueOrOpen
          if flags.is(Flags.Inline) then qualifiers += Qualifier.Inline
          if flags.is(Flags.Transparent) then qualifiers += Qualifier.Transparent
          if flags.is(Flags.Case) then qualifiers += Qualifier.Case

          Icons.Icon(entity, qualifiers.to(List))

        def walk(docs: Docs, ast: Tree, imports: List[Text]): Docs = ast match
          case pc@PackageClause(id@Ident(name), body) =>
            docs.addTerm(body.foldLeft(Docs(name.show, icon(Icons.Entity.Package, pc.symbol.flags).filename))(walk(_, _, t"${name.show}." :: imports)))
            
          case valDef@ValDef(name, rtn, body) if !(valDef.symbol.flags.is(Synthetic) || valDef.symbol.flags.is(Private) || name == "_") =>
            val termName = if valDef.symbol.flags.is(Given) && name.startsWith("given_") then showType(rtn.tpe) else name.show
            docs.addTerm(body.foldLeft(Docs(termName.show, icon(Icons.Entity.Val, valDef.symbol.flags).filename))(walk(_, _, imports)))

          case classDef@ClassDef(name, b, c, None, body) if !classDef.symbol.flags.is(Synthetic | Private | PrivateLocal) =>
            docs.addType(body.foldLeft(Docs(name.show, icon(Icons.Entity.Class, classDef.symbol.flags).filename))(walk(_, _, imports)))

          case classDef@ClassDef(name, b, c, Some(companion), body) if !classDef.symbol.flags.is(Synthetic | Private | PrivateLocal) =>
            if name.endsWith("$package$") then body.foldLeft(docs)(walk(_, _, imports))
            else
              val docs2 = docs.addTerm(body.foldLeft(Docs(if name.show.ends(t"$$") then name.show.drop(1, Rtl) else name.show,
                  icon(Icons.Entity.Class, classDef.symbol.flags).filename))(walk(_, _, imports)))
              walk(docs2, companion, imports)

          case term@DefDef(name, params, rtn, body) if !term.symbol.flags.is(Synthetic) && !term.symbol.flags.is(Private) && !name.contains("$default$") =>
            
            val termName = if term.symbol.flags.is(Given) && name.startsWith("given_") then showType(rtn.tpe) else name.show
            val flags = term.symbol.flags
            if flags.is(Given) then
              docs.addTerm(params.flatMap(_.params).foldLeft(Docs(termName, icon(Icons.Entity.Def, term.symbol.flags).filename))(walk(_, _, imports)))
            else
              docs.addTerm(params.flatMap(_.params).foldLeft(Docs(termName, icon(Icons.Entity.Def, term.symbol.flags).filename))(walk(_, _, imports)))
            
          case typeDef@TypeDef(name, a) if name != "MirroredMonoType" =>
            docs.addType(Docs(name.show, icon(Icons.Entity.Type, typeDef.symbol.flags).filename))

          case Export(name, x) =>
            walk(docs, name, imports)
        
          case other =>
            docs
          
        rootDocs = tastys.foldLeft(rootDocs): (docs, tasty) =>
          walk(docs, tasty.ast, List(t"scala."))
      
      def apply(): Docs = rootDocs
    
    val inspector = DocInspector()
    val files = tastyFiles.to(List).map(summon[GenericFileReader[FileType]].filePath(_))
    
    for file <- files do
      try TastyInspector.inspectTastyFiles(List(file))(inspector)
      catch case err: Exception =>
        println(err)
        err.printStackTrace()
        println(s"Failed to read file $file")

    inspector()
    
