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

import galilei.*, filesystems.unix
import eucalyptus.*
import anticipation.*, fileApi.galileiApi
import honeycomb.*
import cataclysm.{mm as _, *}
import serpentine.*
import parasitism.*, monitors.global
import gossamer.*
import turbulence.*, characterEncodings.utf8
import escapade.*, rendering.ansi
import cellulose.*
import jacinta.*
import scintillate.*
import gesticulate.*
import iridescence.*
import telekinesis.*
import rudiments.{Cursor as _, is as _, *}
import ambience.*, environments.system
import anticipation.*, timeApi.long
import java.util.zip.*
import scala.reflect.*
import scala.quoted.*
import scala.tasty.inspector.*
import scala.tasty.*
import scala.compiletime.*

import unsafeExceptions.canThrowAny

import language.dynamics

import basicIo.jvm
import logging.stdout

given Realm = Realm(t"amok")

val classpath = Classpath()

enum Entity:
  case Trait
  case Class(`abstract`: Boolean)
  case Module
  case OpenClass
  case Given
  case Method
  case Val
  case Var
  case LazyVal
  case Package
  case Type
  case InlineMethod
  case InlineGiven
  case TransInlineMethod
  case TransInlineGiven

object Docs:
  opaque type Dictionary = Map[Text, Docs]

  object Dictionary:
    def apply(): Dictionary = Map()
    
    given Codec[Dictionary] with
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

// object Entity:
//   given Codec[Entity] with
//     def schema: Schema = Field(Arity.One)
    
//     def serialize(value: Entity): List[IArray[cellulose.Node]] =
//       List(IArray(cellulose.Node(value.toString.show.lower)()))
    
//     def deserialize(value: List[Indexed]): Entity throws IncompatibleTypeError =
//       Entity.valueOf(text(value).capitalize.s)
    

case class Docs(name: Text, summary: Maybe[Text] = Unset, doc: Maybe[Text] = Unset,
                    term: Dictionary = Dictionary(), `type`: Dictionary = Dictionary(),
                    value: Maybe[Int] = Unset) extends Dynamic:
  def selectDynamic(id: String): Docs = term.values.find(_.name.s == id).get
  def applyDynamic(id: String)(): Docs = `type`.values.find(_.name.s == id).get
  def addTerm(t: Docs): Docs = copy(term = term.add(t))
  def addType(t: Docs): Docs = copy(`type` = `type`.add(t))
  def empty: Boolean = term.empty && `type`.empty

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
      private var rootDocs: Docs = Docs(t"_root_", Unset, Unset, Dictionary(), Dictionary(), Unset)

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
            case TypeLambda(from, to, tpe)         => t"[${from.mkString(", ")}] =>> ${showType(tpe)}" // FIXME show bounds
            
            case ref: dotty.tools.dotc.core.Types.TypeParamRef =>
              ref.binder match { case TypeLambda(params, _, _) => params(ref.paramNum) }
        
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


        def walk(using Quotes)(docs: Docs, ast: Tree, imports: List[Text]): Docs = ast match
          case pc@PackageClause(id@Ident(name), body) =>
            docs.addTerm(body.foldLeft(Docs(name.show))(walk(_, _, t"${name.show}." :: imports)))
            
          case valDef@ValDef(name, rtn, body) if !(valDef.symbol.flags.is(Synthetic) || valDef.symbol.flags.is(Private) || name == "_") =>
            info(valDef)
            val termName = if valDef.symbol.flags.is(Given) && name.startsWith("given_") then showType(rtn.tpe) else name.show
            docs.addTerm(body.foldLeft(Docs(termName.show))(walk(_, _, imports)))

          case classDef@ClassDef(name, b, c, None, body) if !classDef.symbol.flags.is(Synthetic | Private | PrivateLocal) =>
            docs.addType(body.foldLeft(Docs(name.show))(walk(_, _, imports)))

          case classDef@ClassDef(name, b, c, Some(companion), body) if !classDef.symbol.flags.is(Synthetic | Private | PrivateLocal) =>
            if name.endsWith("$package$") then
              body.foldLeft(docs)(walk(_, _, imports))
            else
              val docs2 = docs.addTerm(body.foldLeft(Docs((if name.show.ends(t"$$") then name.show.drop(1, Rtl) else name.show)))(walk(_, _, imports)))
              walk(docs2, companion, imports)

          case term@DefDef(name, params, rtn, body) if !term.symbol.flags.is(Synthetic) && !term.symbol.flags.is(Private) && !name.contains("$default$") =>
            //println(name+": "+term.symbol.flags.show)
            val termName = if term.symbol.flags.is(Given) && name.startsWith("given_") then showType(rtn.tpe) else name.show
            val flags = term.symbol.flags
            if flags.is(Given) then
              docs.addTerm(params.flatMap(_.params).foldLeft(Docs(termName))(walk(_, _, imports)))
            else
              docs.addTerm(params.flatMap(_.params).foldLeft(Docs(termName))(walk(_, _, imports)))
            
          case typeDef@TypeDef(name, a) if name != "MirroredMonoType" =>
            docs.addType(Docs(name.show))

          case Export(name, x) =>
            walk(docs, name, imports)
        
          case other =>
            //println(other)
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
    
@main
def run(): Unit =
  try
    val dirs = List(
      Unix.parse(t"/home/propensive/work/amok/out").directory(Expect)
    )

    val tastyFiles = dirs.flatMap(_.descendants.filter(_.name.ends(t".tasty")).files)
    
    val docs = Amok.inspect(tastyFiles)
    
    def rewrite(node: CodlNode): CodlNode =
      node.copy(data = node.data.mm { data => data.copy(children = data.children.map(rewrite)) }).promote(1)

    val codec = summon[Codec[Docs]]
    val codl = CodlDoc(IArray.from(codec.serialize(docs).flatten).map(rewrite), codec.schema, 0)
    //println(codl.serialize)

    val count = Counter(0)
  
    def render(docs: Docs, prefix: Text = t"i", path: List[Text] = Nil): List[Element["li"]] =
      if docs.term.values.size + docs.`type`.values.size > 0
      then (docs.term.values ++ docs.`type`.values).groupBy(_.name).to(List).sortBy(_(0)).zipWithIndex.map:
        case ((name, items), idx) =>
          val empty = items.forall(_.empty)
          Li(tabindex = count(), hclass = if empty then cls"" else more)(
            Label(`for` = t"$prefix-$idx")(A(href = ^ / p"info" / path.reverse.join(t"", t".", t".$name"), target = t"main")(name)),
            Input(id = t"$prefix-$idx", htype = HType.Checkbox),
            if empty then Nil else List(Ul(items.flatMap(render(_, t"$prefix-$idx", name :: path)).to(List)))
          )
      else Nil
    
    val html = HtmlDoc(
      Html(
        Head(
          Title(t"Amok Documentation"),
          Link(rel = Rel.Stylesheet, href = ^ / p"styles" / p"amok.css"),
          Link(rel = Rel.Icon, htype = media"image/svg+xml", href = ^ / p"images" / p"logo.svg")
        ),
        Body(
          Header(Ul(
            Li(A(href = ^)(t"HOME")),
            Li(A(href = ^ / p"about")(t"ABOUT AMOK")),
            Li(A(href = ^ / p"kill")(t"SHUTDOWN"))
          )),
          Main(Iframe(name = t"main", src = ^ / p"info" / p"welcome")),
          Nav(
            H2(t"API Documentation"),
            Input(name = t"filter"),
            Ul(render(docs))
          ),
          Footer()
        )
      )
    )
    
    def infoPage(name: Text) = HtmlDoc(
      Html(
        Head(
          Title(t"$name"),
          Link(rel = Rel.Stylesheet, href = ^ / p"styles" / p"amok.css")
        ),
        Body(
          H1(Code(t"$name")),
          H2(t"About Amok"),
          P(t"Welcome to Amok, an API tool for Scala and other languages. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."),
          Pre(t"This is some code.")
        )
      )
    )

    def font(name: Text): Bytes = (classpath / p"amok" / p"fonts" / name).read[Bytes]
    def image(name: Text): Text = (classpath / p"amok" / p"images" / name).read[Text]

    lazy val server: ActiveServer = HttpServer(8080).listen:
      request.path match
        case ^ / t"styles" / t"amok.css" => Response(css)
        case ^ / t"fonts" / name         => Response(Ttf(font(name)))
        case ^ / t"images" / name        => Response(Svg(image(name)))
        case ^ / t"info" / name          => Response(infoPage(name))
        case _                           => Response(html)
    
    server.task.await()
    
    Log.info("Server completed running")


    //HtmlDoc.serialize(html).writeTo(Unix.parse(t"/home/propensive/dev/amok/out.html").file(Ensure))
  catch case err: Throwable =>
    println(err.toString+" at "+err.getStackTrace().nn.to(List).mkString("\n"))

import pseudo.*

val more = cls"more"

val css = CssStylesheet(
  FontFace(fontFamily = t"\"Overpass\"", src = t"url(\"../fonts/overpass.ttf\")"),
  FontFace(fontFamily = t"\"Overpass Mono\"", src = t"url(\"../fonts/overpass-mono.ttf\")"),
  FontFace(fontFamily = t"\"Overpass Italic\"", src = t"url(\"../fonts/overpass-italic.ttf\")"),
  
  select(Body):
    Css(fontFamily = Font(t"Overpass"), margin = 0, padding = 0, overflowY = Overflow.Hidden, textAlign = TextAlign.Justify),

  select(Label):
    Css(fontWeight = 400, userSelect = UserSelect.None, fontFamily = Font(t"Overpass Mono"),
        fontVariantLigatures = t"none", fontSize = 0.9.em, width = 400.px,
        cursor = Cursor.Pointer, padding = (0.2.em, 4.5.em), marginLeft = -4.em),
  
  select(Header):
    Css(position = Position.Absolute, margin = 0, height = 6.em, width = 100.vw - 6.em, padding = (0, 0, 0, 6.em),
        backgroundColor = rgb"#111111", boxShadow = (0, 0, 1.em, rgb"#aaaaaa"),
        backgroundImage = ^ / p"images" / p"logo.svg",
        backgroundRepeat = t"no-repeat", backgroundSize = 4.em, backgroundPosition = t"1em"),
  
  select(Header >> A || Header >> A&&hover || Header >> A&&visited || Header >> A&&active):
    Css(color = rgb"#dddddd", textDecoration = TextDecorationLine.None, fontWeight = 800, fontSize = 0.85.em),
  
  select(Header >> Ul):
    Css(margin = (1.4.em, 0)),
  
  select(Header >> Li):
    Css(display = Display.InlineBlock, padding = 1.em),
  
  select(Main):
    Css(position = Position.Absolute, height = 100.vh - 8.em, width = 54.vw - 4.em, padding = (0, 2.em),
        margin = (8.em, 0, 0, 42.vw), overflowY = Overflow.Scroll),
  
  select(Iframe):
    Css(width = 100.pc, height = 100.pc, borderStyle = BorderStyle.None),

  select(Nav):
    Css(position = Position.Absolute, height = 100.vh - 8.em, margin = (8.em, 0, 0, 0), width = 38.vw,
        padding = (0, 2.em, 0, 2.em), overflowY = Overflow.Scroll),
  
  select(Ul):
    Css(listStyle = t"none", padding = 0),

  select(H1 > Code):
    Css(fontFamily = Font(t"Overpass Mono"), fontWeight = 500, fontSize = 0.8.em),
  
  select(P):
    Css(fontWeight = 325, fontSize = 0.95.em, lineHeight = 1.5.em, color = rgb"#444444"),

  select(H2):
    Css(color = rgb"#555555", fontWeight = 400, fontSize = 1.2.em),

  select(Nav >> Li):
    Css(paddingLeft = 1.7.em, overflowX = Overflow.Hidden),
  
  select(Nav >> A):
    Css(color = rgb"#000000", textDecoration = TextDecorationLine.None),
  
  select(Nav >> A&&hover):
    Css(textDecoration = TextDecorationLine.Underline),

  select(Li > Label):
    Css(lineHeight = 1.5.em, backgroundImage = ^ / p"images" / p"m.svg", backgroundRepeat = t"no-repeat",
        backgroundPosition = t"3.5em -0.1em",
        backgroundSize = 1.3.em, overflowX = Overflow.Hidden, padding = (0.em, 0.em, 0.em, 5.5.em)),
  
  select(Nav >> Li && more):
    Css(backgroundImage = ^ / p"images" / p"more.svg", backgroundSize = 1.3.em, backgroundRepeat = t"no-repeat", backgroundPosition = t"-0.35em -0.1em"),
  
  select(Nav >> Li && more && has(Input&&checked)):
    Css(backgroundImage = ^ / p"images" / p"less.svg", backgroundSize = 1.3.em, backgroundRepeat = t"no-repeat", backgroundPosition = t"-0.35em -0.1em"),
  
  select(Nav >> Li > Ul):
    Css(marginLeft = 0.em),
  
  select(Nav > Input):
    Css(borderStyle = BorderStyle.None, borderBottom = (BorderStyle.Solid, 1.px, rgb"#777777"),
        fontFamily = Font(t"Overpass Mono"), fontSize = 0.9.em, width = 20.em, marginLeft = -0.3.em, padding = (0, 0, 0.1.em, 1.9.em),
        backgroundImage = ^ / p"images" / p"filter.svg", backgroundSize = 1.3.em, backgroundRepeat = t"no-repeat", backgroundPosition = t"0.1em -0.2em"),
  
  select(Nav > Input&&focus):
    Css(borderColor = rgb"#000000", outline = t"none"),
    
  select(Ul >> Input):
    Css(display = Display.None),
  
  select(Li && hover):
    Css(fontWeight = 700),
  
  select(Ul >> Input ~ Ul > Li):
    Css(minHeight = Inherit, height = Inherit, maxHeight = 0.px, overflowY = Overflow.Hidden, transition = t"all ease-in-out 0.4s"),
  
  select(Ul >> Input&&checked ~ Ul > Li):
    Css(minHeight = 20.px, height = Inherit, maxHeight = Inherit),
  
  cataclysm.Media(t"only screen and (max-width: 1000px)")(
    select(Main):
      Css(height = 50.vh, margin = (50.vh, 0, 0, 0), width = 100.vw - 4.em,
          borderTop = (BorderStyle.Solid, 1.px, rgb"#dddddd")),
    
    select(Nav):
      Css(height = 50.vh - 8.em, margin = (8.em, 0, 0, 0), width = 100.vw - 4.em)
  ),
  cataclysm.Media(t"only screen and (max-device-width: 768px)")(
    select(Main):
      Css(height = 50.vh, margin = (50.vh, 0, 0, 0), width = 100.vw - 4.em,
          borderTop = (BorderStyle.Solid, 1.px, rgb"#dddddd")),
    
    select(Nav):
      Css(height = 50.vh - 8.em, margin = (8.em, 0, 0, 0), width = 100.vw - 4.em)
  )
)

enum TypeEntry:
  case CaseClass, AbstractClass, AbstractOpenClass, Trait, SealedTrait, TypeAlias, OpaqueTypeAlias,
      OpenClass, Class, TransparentSealedTrait