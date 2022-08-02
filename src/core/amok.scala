package amok

import joviality.*
import serpentine.*
import gossamer.*
import rudiments.*, environments.system
import anticipation.*, timekeeping.long
import java.util.zip.*
import scala.collection.JavaConverters.*
import scala.reflect.*
import scala.quoted.*
import scala.tasty.inspector.*
import scala.tasty.*

import unsafeExceptions.canThrowAny

object Model:
  enum Term:
    case Object(name: Text)
    case Val(isLazy: Boolean, isAbstract: Boolean, name: Text)
    case Def(isAbstract: Boolean, name: Text, paramLists: List[List[Param]])
  
  case class Param()

  enum Type:
    case Class(name: Text)
    case Trait(name: Text)
    case Alias(name: Text)

object Doc:
  case class Signature(path: Path, params: List[List[Doc.Param]] = Nil, returnType: Option[Doc.Path] = None):
    override def toString(): String = text.s
    def text: Text =
      val first = path.text+params.map(_.map(_.text).join(t"(", t", ", t")")).join
      val rtype = returnType.fold(t"")(t": "+_.text)
      
      first+rtype

  case class Path(elements: Vector[Doc.Name]):
    def /(name: Name): Path = Path(elements :+ name)
    
    def ioPath(root: DiskPath[Unix]): DiskPath[Unix] = elements match
      case Name.Term(term) +: tail =>
        Path(tail).ioPath(root / term)
      
      case Name.Type(typ) +: tail =>
        Path(tail).ioPath(root / t"+$typ")
      
      case empty =>
        val filename = if root.name.startsWith(t"+") then root.name.drop(1) else root.name
        root.parent / t"$filename.md"
    
    def text: Text = elements match
      case name +: Vector()        => name.text
      case Name.Term(term) +: tail => t"$term."+Path(tail).text
      case Name.Type(typ) +: tail  => t"$typ#"+Path(tail).text
    
  enum Name:
    case Term(name: Text)
    case Type(name: Text)

    def text: Text = this match
      case Term(name) => name
      case Type(name) => name

  case class Param(name: Option[Text], tpe: Doc.Name.Type):
    override def toString: String = (name.fold(t"")(_+t": ")+tpe.text).s
    def text: Text = toString.show
  
  case class TypeParam(name: Text):
    override def toString: String = name.s
    def text: Text = name

case class DocInspector(out: Directory[Unix]) extends Inspector:
  def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
    import quotes.reflect.*

    def makeType(tree: Tree): Doc.Name.Type = tree match
      case TypeIdent(id) => Doc.Name.Type(id.show)
      case Applied(a, b) => Doc.Name.Type(makeType(a).text+b.map(makeType(_).name).join(t"[", t", ", t"]"))
      case Annotated(a, b) => Doc.Name.Type(t"annotated "+b.toString.show)
      case TypeBoundsTree(a, b) => Doc.Name.Type(makeType(a).name+t"  "+b.toString.show)
      //case AppliedTypeTree(t, params) => makeType(tree).text+params.map(makeType(_).map(_.text)).mkString("[", ", ", "]")
      case other     => Doc.Name.Type(other.getClass.toString.show)

    def walk(using Quotes)(ast: Tree, path: Doc.Path): Vector[Doc.Signature] =
      ast match
        case pc@PackageClause(id@Ident(pkg), body) =>
          body.to(Vector).flatMap(walk(_, path / Doc.Name.Term(pkg.toString.show)))
        
        case term@ValDef(name, rtn, body) if !term.symbol.flags.is(Flags.Synthetic | Flags.Private) =>
          val newPath = path / Doc.Name.Term(name.show)
          Vector(Doc.Signature(newPath, returnType = None))//Some(makeType(rtn))))

        case classDef@ClassDef(name, b, c, None, body) if !classDef.symbol.flags.is(Flags.Synthetic) && !(name.contains(t"$$")) =>
          val newPath = path / Doc.Name.Type(name.show)
          body.to(Vector).flatMap(walk(_, newPath)) :+ Doc.Signature(newPath)

        case classDef@ClassDef(name, b, c, Some(companion), body) if !classDef.symbol.flags.is(Flags.Synthetic) =>
          val newPath = path / Doc.Name.Term(if name.show.endsWith(t"$$") then name.show.drop(1, Rtl) else name.show)

          body.to(Vector).flatMap(walk(_, newPath)) :+ Doc.Signature(newPath)
        
        case term@DefDef(name, params, rtn, body) if !term.symbol.flags.is(Flags.Synthetic) =>
          val newPath = path / Doc.Name.Term(name.show)
          Vector(Doc.Signature(newPath, params.map(_.params.map:
            case term@ValDef(name, rtn, c) =>
              Doc.Param(if term.symbol.flags.is(Flags.Synthetic) then None else Some(name.show), makeType(rtn))
            case TypeDef(name, rtn) =>
              Doc.Param(Some(name.show), makeType(rtn))
          )))
        
        case typeDef@TypeDef(name, a) =>
          val newPath = path / Doc.Name.Type(name.show)
          Vector(Doc.Signature(newPath))
        case Export(name, x) => 
          Vector()
        case DefDef(_, _, _, _) => Vector()
        case Import(_, _) => Vector()
        case ValDef(_, _, _) => Vector()
        case ClassDef(_, _, _, _, _) => Vector()
        case other =>
          Vector()
      
    val sigs = for tasty <- tastys yield walk(tasty.ast, Doc.Path(Vector()))
    sigs.flatten.foreach:
      sig =>
        val path = sig.path.ioPath(out.path)
        if !path.parent.exists() then path.parent.directory(Create)

        if !path.exists() then
          println(path.show)
          path.file(Create)
    sigs

@main
def run(files: Text*): Unit =
try
  val userDir = pwd[Unix]()
  val dirs = files.init.map(Unix.parse(_, userDir).to(LazyList)).flatten.map(_.directory(Ensure))
  val out: DiskPath[Unix] = Unix.parse(files.last, userDir).get
  val dir: Directory[Unix] = out.directory(Ensure)
  val t0 = now()
  val tastyFiles: List[String] = dirs.flatMap(_.files).filter(_.name.endsWith(t".tasty")).to(List).map(_.fullname.s)
  val signatures = TastyInspector.inspectTastyFiles(tastyFiles)(DocInspector(dir))
  val t1 = System.currentTimeMillis
  println((t1 - t0).toString+"ms")
catch case err: Exception =>
  err.printStackTrace()
  sys.exit(1)
