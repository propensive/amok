package amok

import galilei.*, filesystems.unix
import serpentine.*
import eucalyptus.*
import anticipation.*, integration.galileiPath
import honeycomb.*
import parasitism.*, monitors.global, threading.platform
import gossamer.*, stdouts.stdout
import turbulence.*
import escapade.*, rendering.ansi
import cellulose.*
import euphemism.*
import rudiments.{is => _, *}, environments.system
import anticipation.*, timekeeping.long
import java.util.zip.*
import scala.reflect.*
import scala.quoted.*
import scala.tasty.inspector.*
import scala.tasty.*

import unsafeExceptions.canThrowAny

import language.dynamics

val logDest = Unix.parse(t"/home/propensive/amok.log").file(Ensure).sink
given Log({ case _ => logDest })
given Realm(t"amok")


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

object Amok:
  def inspect[F: FileInterpreter](tastyFiles: Seq[F]): Docs =
    case class DocInspector() extends Inspector:
      private var rootDocs: Docs = Docs(t"_root_", Unset, Unset, Dictionary(), Dictionary(), Unset)
      def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
        import quotes.reflect.*
        import Flags.*

        def walk(using Quotes)(docs: Docs, ast: Tree, imports: List[Text]): Docs = ast match
          case pc@PackageClause(id@Ident(name), body) =>
            docs.addTerm(body.foldLeft(Docs(name.show))(walk(_, _, t"${name.show}." :: imports)))
            
          case term@ValDef(name, rtn, body) if !term.symbol.flags.is(Synthetic | Private) =>
            if name.show.ends(t"$$package") then body.foldLeft(docs)(walk(_, _, imports))
            else docs.addTerm(body.foldLeft(Docs(name.show))(walk(_, _, imports)))

          case classDef@ClassDef(name, b, c, None, body) if !classDef.symbol.flags.is(Synthetic) =>
            docs.addType(body.foldLeft(Docs(name.show))(walk(_, _, imports)))

          case classDef@ClassDef(name, b, c, Some(companion), body) if !classDef.symbol.flags.is(Synthetic) =>
            val docs2 = docs.addTerm(body.foldLeft(Docs((if name.show.ends(t"$$") then name.show.drop(1, Rtl) else name.show)))(walk(_, _, imports)))
            walk(docs2, companion, imports)

          case term@DefDef(name, params, rtn, body) if !term.symbol.flags.is(Synthetic) =>
            val flags = term.symbol.flags
            if flags.is(Given) then
              docs.addTerm(params.flatMap(_.params).foldLeft(Docs(name.show))(walk(_, _, imports)))
            else
              docs.addTerm(params.flatMap(_.params).foldLeft(Docs(name.show))(walk(_, _, imports)))
            
          case typeDef@TypeDef(name, a) =>
            docs.addType(Docs(name.show))

          case Export(name, x) =>
            walk(docs, name, imports)
        
          case DefDef(_, _, _, _) =>
            docs

          case Import(_, _) =>
            docs
          
          case ValDef(_, _, _) =>
            docs
          
          case ClassDef(_, _, _, _, _) =>
            docs
          
          case other =>
            //Log.info(t"Found ${other.toString}")
            docs
          
        rootDocs = tastys.foldLeft(rootDocs): (docs, tasty) =>
          walk(docs, tasty.ast, List(t"scala."))
      
      def apply(): Docs = rootDocs
    val inspector = DocInspector()
    val files = tastyFiles.to(List).map(summon[FileInterpreter[F]].filePath(_))
    for file <- files do
      try TastyInspector.inspectTastyFiles(List(file))(inspector)
      catch case err: Exception => println(s"Failed to read file $file")

    inspector()
    
@main
def run(): Unit =
  try
    val dir = Unix.parse(t"/home/propensive/.cache/irk/cls/gossamer/core").directory(Expect)
    val tastyFiles = dir.descendants.filter(_.name.ends(t".tasty")).files
    
    val docs = Amok.inspect(tastyFiles)
    
    def rewrite(node: Nodule): Nodule =
      node.copy(data = node.data.mm { data => data.copy(children = data.children.map(rewrite)) }).promote(1)

    val codec = summon[Codec[Docs]]
    val codl = CodlDoc(IArray.from(codec.serialize(docs).flatten).map(rewrite), codec.schema, 0)
    println(codl.serialize)

    val count = Counter(0)
  
    def render(docs: Docs): List[Element["ul"]] =
      if docs.term.values.size + docs.`type`.values.size > 0 then List(Ul(
        (docs.term.values ++ docs.`type`.values).to(List).sortBy(_.name).map: item =>
          Li(tabindex = count())(item.name, render(item))
        .to(List)
      )) else Nil
  
    val html = HtmlDoc(
      Html(
        Head(
          Title(t"Amok Documentation"),
          Link(rel = Rel.Stylesheet, href = Relative.Self / p"styles.css")
        ),
        Body(
          Header(Ul(
            Li(A(href = ^)(t"Home")),
            Li(A(href = ^ / p"about")(t"About"))
          )),
          Nav(render(docs)),
          Main(),
          Footer()
        )
      )
    )
  
    HtmlDoc.serialize(html).writeTo(Unix.parse(t"/home/propensive/dev/amok/out.html").file(Ensure))
  catch case err: Exception =>
    println(err.toString+" at "+err.getStackTrace().nn.to(List).mkString("\n"))
