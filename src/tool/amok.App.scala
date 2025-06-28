package amok

import scala.tasty.*, inspector.*
import scala.quoted.*
import scala.collection.mutable as scm

import soundness.{is as _, *}

import executives.completions
import unhandledErrors.stackTrace
import parameterInterpretation.posix
import threadModels.platform
import workingDirectories.daemonClient
import logging.silent
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.disabled
import filesystemOptions.createNonexistent.disabled
import alphabets.base64.standard
import treeStyles.default
import httpServers.stdlibPublic
import asyncTermination.cancel
import supervisors.global
import charEncoders.utf8
import charDecoders.utf8
import textSanitizers.skip
import classloaders.threadContext

given Tactic[CodlError] => Tactic[CodlReadError] => Translator =
  HtmlTranslator(ScalaEmbedding)

val About = Subcommand(t"about", e"find out about Amok")
val Load = Subcommand(t"load", e"load definitions from a .jar or .amok file")
val Clear = Subcommand(t"clear", e"clear definitions from a JAR file")
val Quit = Subcommand(t"quit", e"shutdown Amok")
val Serve = Subcommand(t"serve", e"serve the documentation on a local HTTP server")

var amokDb = AmokDb()


object AmokData:
  case class Base(base: Text, memo: Optional[Text], detail: Optional[Text], entry: List[Entry])

  object Entry:
    given decoder: Void => CodlDecoder[Entry] = CodlDecoder.derived
    given encoder: Void => CodlEncoder[Entry] = CodlEncoder.derived

  case class Entry(name: Text, memo: Optional[Text], detail: Optional[Text], refer: List[Text], entry: List[Entry])

  def read(file: Path on Linux)(using Stdio): Base raises CodlError raises CodlReadError raises IoError raises StreamError =
    Out.println(m"File ${file.encode} exists? ${file.exists().toString}")
    file.open: file =>
      Codl.read[Base](file)


@main
def application(): Unit = cli:
  idempotent(effectful(safely(TabCompletions.install())))

  arguments match
    case About() :: _ => execute:
      recover:
        case SerializationError(_, _) => Out.println(m"Failed to deserialize")

      . within:
          Out.println()
          t"""H4sIADMTXWgAA51RwQ3AIAj8OwWjNr77lDFqnYlJGoO2ojaChhjEu+NQAO0ivCgcJTA6NREoeN63OKLvGBh7Z4
              RnhxgroFCtDobJCdMEX7DMYgZX23yO+KrBZ694/89e71p/pqx8Zu7iFl6smdR51ZPd95oz+4XunQTCet5RdA9q
              ts7STgMAAA=="""
          . erase(' ', '\n')
          . deserialize[Base64]
          . gunzip
          . utf8
          . cut(t"\n")
          . each: line =>
              Out.println(line)

          Out.println(e"$Bold(Amok) version 1.0.0: $Italic(a documentation compiler for Scala)")
          Out.println(e"© Copyright 2025, Propensive OÜ")
          Out.println()

      Exit.Ok

    case Load() :: Pathname(file) :: _ =>
      execute:
        if file.name.ends(t".jar") then
          recover:
            case IoError(_, _, _) => Exit.Fail(1)
            case StreamError(_)   => Exit.Fail(1)

          . within:
              amokDb.load(file)
              Exit.Ok
        else if file.name.ends(t".amok") then
          recover:
            case exception: Error =>
              Out.println(exception.stackTrace.teletype)
              Exit.Fail(1)
          . within:
              amokDb.overlay(AmokData.read(file))
              Exit.Ok

        else Exit.Ok

    case Clear() :: Nil =>
      execute:
        amokDb = AmokDb()
        Out.println(m"Documentation database has been cleared")
        Exit.Ok

    case Quit() :: _ => execute(service.shutdown() yet Exit.Ok)

    case Serve() :: _ => execute:
      recover:
        case ServerError(port) =>
          Out.println(m"Can't start a server on port $port") yet Exit.Fail(1)
        case ClasspathError(path) =>
          panic(m"Expected to find $path on the classpath")

      . within:
          tcp"8080".serve:
            request.location match
              case _ /: t"api.css"  => Http.Response(Classpath/"amok"/"api.css")
              case _ /: t"styles.css"  => Http.Response(Classpath/"amok"/"styles.css")
              case _ /: t"logo.svg" => Http.Response(Classpath/"amok"/"logo.svg")

              case _ /: t"entity" /: (name: Text) =>
                val (prefix, entity, item) = amokDb.resolve(name)
                Http.Response:
                  import html5.*
                  Page.simple
                   (H2(t"trait"),
                    H3(prefix),
                    H1(entity),
                    Div(safely(item.memo.let(Markdown.parse(_).html)).or(Nil)),
                    Div(safely(item.detail.let(Markdown.parse(_).html)).or(Nil)))

              case _ /: t"package" /: (pkg: Text) =>
                import html5.*

                def tree(name: Text, item: Item, group: Text, path: Text): Element["details"] =
                  val members = item.members

                  // FIXME: This needs to be much clearer
                  val location =
                    (% / "entity" / path.asInstanceOf[Name[Rfc3986]]).on[Rfc3986]



                  Details(name = group.urlEncode)
                   (if members.isEmpty then Summary(A(href = location, target = id"main")(name))
                    else Summary.full(A(href = location, target = id"main")(name)),
                    Div.content:
                      members.map: (member, item) =>
                        tree(member.text, item, path, path+member.safe))

                Http.Response:
                  Page
                   (Details(Summary(B(pkg))),
                    Div.content:
                      amokDb(pkg).members.map: (member, item) =>
                        tree(member.text, item, pkg, pkg+member.safe))

              case _ =>
                Http.Response(t"Hello")

          Exit.Ok

    case _ =>
      execute(Out.println(m"Unknown command") yet Exit.Fail(1))


object Page:
  import html5.*

  def apply(nav: Html[Flow]*): HtmlDoc = HtmlDoc:
    Html
      (Head
        (Link.Stylesheet(href = % / "api.css")),
      Body
        (Header(Ul(Li(A(href = % / "docs")(t"docs")))),
        Main
         (Nav(Div.menu(nav*)),
          Article(Iframe(name = t"main", width = 640))),
        Footer(t"Copyright 2025, Propensive OÜ")))

  def simple(content: Html[Flow]*): HtmlDoc = HtmlDoc:
    Html(Head(Link.Stylesheet(href = % / "styles.css")), Body(content*))

class Item():
  val membersMap: scm.TreeMap[Member, Item] = scm.TreeMap()
  private var doc: Optional[Text] = Unset
  var memo: Optional[Text] = Unset
  var detail: Optional[Text] = Unset

  def members: List[(Member, Item)] = membersMap.to(List)

  override def toString: String = members.map(_(0).text).join(t", ").s

  def apply(member: Member): Item = membersMap.get(member).getOrElse:
    Item().tap: item =>
      membersMap(member) = item

object Member:
  given ordering: Ordering[Member] = Ordering.by[Member, String]:
      case Member.Root(left)   => left.s
      case Member.OfType(left) => left.s
      case Member.OfTerm(left) => "."+left.s

enum Member:
  case OfTerm(name: Text)
  case OfType(name: Text)
  case Root(name: Text)

  def text: Text = this match
    case OfTerm(name) => t".$name"
    case OfType(name) => t"⌗$name"
    case Root(name)   => name

  def safe: Text = this match
    case OfTerm(name) => t".${name.urlEncode}"
    case OfType(name) => t":${name.urlEncode}"
    case Root(name)   => name

class AmokDb():
  val root = Item()

  def resolve(path: Text, prefix: Text = t"", current: Item = root, term: Boolean = true)
  : (Text, Text, Item) =

      path.where { char => char == '.' || char == ':' }.let: position =>
        val part = path.before(position).urlDecode
        val next = current(if term then Member.OfTerm(part) else Member.OfType(part))

        path.at(position) match
          case '.' => resolve(path.after(position), t"$prefix$part.", next, true)
          case ':' => resolve(path.after(position), t"$prefix$part⌗", next, false)
          case _   => (prefix, part, current)

      . or:
         (prefix,
          path.urlDecode,
          current(if term then Member.OfTerm(path.urlDecode) else Member.OfType(path.urlDecode)))

  def apply(pkg: Text): Item = root(Member.OfTerm(pkg))

  def overlay(base: AmokData.Base)(using Stdio): Unit =
    def recur(prefix: Text, entries: List[AmokData.Entry], current: Item): Unit =
      entries.map: entry =>
        val part = entry.name.skip(1)
        Out.println(t"On ${current.toString}, adding ${entry.name}")
        val next = entry.name.at(Prim) match
          case '.' => current(Member.OfTerm(part))
          case '#' => current(Member.OfType(part))
          case other   => Out.println(m"Unexpected: ${other.inspect}") yet Unset
        Out.println(t"    Now on ${next.toString}")
        Out.println()

        next.let: next =>
          next.memo = entry.memo
          next.detail = entry.detail
          recur(prefix+entry.name, entry.entry, next)

    recur(base.base, base.entry, root(Member.OfTerm(base.base)))

  def load(path: Path on Linux)(using Stdio): Unit =
    val inspector = DocInspector()
    try TastyInspector.inspectTastyFilesInJar(path.encode.s)(inspector)
    catch case error: Throwable =>
      Out.println(error.stackTrace.teletype)

  import Member.{OfTerm, OfType}

  object Scope:
    given scope: Scope = Scope(Set(
      List(t"_root_", t"scala"),
      List(t"_root_", t"scala", t"caps")
    ))

  case class Scope(packages: Set[List[Text]]):
    def prefix(path: List[Text]): Text =
      if packages.contains(path) then t"" else t"${path.join(t".")}."

  case class DocInspector()(using Stdio) extends Inspector:
    def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
      import quotes.reflect.*
      import Flags.*

      val retainsSym = TypeRepr.of[annotation.retains].typeSymbol
      val any = TypeRepr.of[Any]
      val nothing = TypeRepr.of[Nothing]

      def pname(term: Term, xs: List[Text] = Nil): List[Text] = term match
        case Ident(base)        => base.show :: xs
        case Select(parent, id) => pname(parent, id.show :: xs)

      def showType(using scope: Scope)(repr: quotes.reflect.TypeRepr, parens: Boolean = false): Text =
        repr.absolve match
          case AppliedType(base, args)           =>
            val argTypes = args.map(showType(_)).join(t", ")
            if defn.isTupleClass(base.typeSymbol) then t"($argTypes)"
            else
              if args.length == 2 && repr.typeSymbol.flags.is(Infix)
              then t"${showType(args(0))} ${showType(base)} ${showType(args(1))}"
              else t"${showType(base)}[$argTypes]"

          case ConstantType(IntConstant(int))       => int.show
          case ConstantType(LongConstant(long))     => long.show
          case ConstantType(BooleanConstant(true))  => t"true"
          case ConstantType(BooleanConstant(false)) => t"false"
          case ConstantType(StringConstant(str))    => t"\"$str\""
          case TypeRef(prefix, ref)                 => ref.show
          case AnnotatedType(tpe, anns)             => t"${captures(anns)}${showType(tpe)}"
          case OrType(left, right)                  => t"${showType(left, true)} | ${showType(right, true)}"
          case AndType(left, right)                 => t"${showType(left, true)} & ${showType(right, true)}"
          case TermRef(prefix, name)                => t"$name.type"
          case TypeLambda(from, to, tpe)            => t"[${from.mkString(", ")}] =>> ${showType(tpe, true)}"
          case TypeBounds(lb, ub)                   =>
            if lb == ub then showType(lb)
            else if lb == nothing && ub == any then t"?"
            else if lb == nothing then t"? <: ${showType(ub, true)}"
            else if lb == any then t"? >: ${showType(lb, true)}"
            else t"? >: ${showType(lb, true)} <: ${showType(ub, true)}}"

          case Refinement(parent, name, infos) =>
            if name == "Self" then t"${showType(infos)} is ${showType(parent)}"
            else t"{showType(parent)} { type $name = ${showType(infos)} }"

          case ref: dotty.tools.dotc.core.Types.TypeParamRef =>
            ref.binder match { case TypeLambda(params, _, _) => params(ref.paramNum).show }

          case other =>
            t"???"

      def captures(using scope: Scope)(term: quotes.reflect.Term): Text =
        term match
          case Apply(Select(New(focus), _), List(Typed(Repeated(values, _), _))) if focus.tpe.typeSymbol == retainsSym =>
            values.map:
              case Select(prefix, name) => t"${scope.prefix(pname(prefix))}${name}"
              case Ident(name)          => t"${name}"

            . join(t"{", t", ", t"} ")

          case other => t""

      def walk(ast: Tree, item: Item, ofTerm: Boolean): Unit =
        def of(name: Text): Member = if ofTerm then OfTerm(name) else OfType(name)
        ast match
          case pc@PackageClause(id@Ident(name), body) =>
            val child = item(of(name))
            body.each(walk(_, child, true))

          case valDef@ValDef(name, rtn, body) if !(valDef.symbol.flags.is(Private) || name == "_") =>
            val termName = if valDef.symbol.flags.is(Given) && (name.startsWith("evidence$")) then showType(rtn.tpe) else name.show
            if termName.ends(t"$$package") then body.each(walk(_, item, true))
            else
              val child = item(of(termName))
              body.each(walk(_, child, true))

          case classDef@ClassDef(name, defDef, _, selfType, body) =>
            val obj = name.endsWith("$")
            if name.endsWith("$package") || name.endsWith("$package$") then
              body.each(walk(_, item, obj))
            else
              val obj = name.endsWith("$")
              val className = if obj then name.show.skip(1, Rtl) else name.show
              val child = item(of(className))
              body.each(walk(_, child, obj))

          case term@DefDef(name, params, rtn, body) if !term.symbol.flags.is(Synthetic) && !term.symbol.flags.is(Private) && !name.contains("$default$") =>
            val termName = if term.symbol.flags.is(Given) && name.startsWith("given_") then showType(rtn.tpe) else name.show
            val child = item(of(termName))
            //params.flatMap(_.params).each(walk(_, child, true))

          case typeDef@TypeDef(name, a) if name != "MirroredMonoType" =>

          case Export(_, exports) => exports.map:
            case SimpleSelector(name)    => item(of(name.tt))
            case RenameSelector(_, name) => item(of(name.tt))

          case other =>
            //Out.println(other.toString.tt)
            ()

      tastys.each: tasty =>
        walk(tasty.ast, root, true)

      TreeDiagram.by[(Member, Item)](_(1).members)(OfTerm(t"jacinta") -> root(OfTerm(t"jacinta"))).render: (member, item) =>
          t"▪ "+member.text
      . strict
      . each(Out.println(_))
