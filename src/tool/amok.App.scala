package amok

import scala.tasty.*, inspector.*
import scala.quoted.*

import soundness.{is as _, Node as _, *}

given Tactic[CodlError] => Tactic[CodlReadError] => Translator =
  HtmlTranslator(AmokEmbedding(false), ScalaEmbedding)

val About  = Subcommand(t"about",  e"find out about Amok")
val Load   = Subcommand(t"load",   e"load definitions from a .jar or .amok file")
val Clear  = Subcommand(t"clear",  e"clear definitions from a JAR file")
val Quit   = Subcommand(t"quit",   e"shutdown Amok")
val Serve  = Subcommand(t"serve",  e"serve the documentation on a local HTTP server")

var model = Model()

given Imports(Set
       (Index.decode(t"scala.Predef"),
        Index.decode(t"prepositional"),
        Index.decode(t"java.lang")))

@main
def application(): Unit = cli:
  idempotent(effectful(safely(TabCompletions.install())))

  arguments match
    case About() :: _ => execute:
      recover:
        case SerializationError(_, _) => panic(m"Failed to deserialize")

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

          given Decimalizer(significantFigures = 4, exponentThreshold = Unset)
          val memory = Heap.used/1.mib
          val build = unsafely((Classpath / "build.id").read[Text].trim)

          Out.println(e"$Bold(Amok) prerelease version, build $build: $Italic(a documentation compiler for Scala)")
          Out.println(e"© Copyright 2025, Propensive OÜ")
          Out.println()
          Out.println(e"Memory usage: $memory MiB")
          Out.println()

      Exit.Ok

    case Load() :: Pathname(file) :: _ =>
      execute:
        if file.name.ends(t".jar") then
          recover:
            case IoError(_, _, _) => Exit.Fail(1)
            case StreamError(_)   => Exit.Fail(1)

          . within:
              model.load(file)
              Exit.Ok

        else if file.name.ends(t".amok") then
          recover:
            case exception: Error =>
              Out.println(exception.stackTrace.teletype)
              Exit.Fail(1)
          . within:
              model.overlay(Amox.read(file))
              Exit.Ok

        else Exit.Ok

    case Clear() :: Nil =>
      execute:
        model = Model()
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
          httpServer()
          Out.println(e"Listening on $Bold(http://localhost:8080)")
          Exit.Ok

    case command :: _ =>
      execute(Out.println(m"Unknown command: ${command()}") yet Exit.Fail(1))


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

class Model():
  val root = Node()

  def resolve(path: Text, prefix: Text = t"", current: Node = root, term: Boolean = true)
  : (Text, Text, Node) =

      path.where { char => char == '.' || char == ':' }.let: position =>
        val part = path.before(position).urlDecode
        val next = current(if term then Member.OfTerm(part) else Member.OfType(part))

        path.at(position) match
          case '.' => resolve(path.after(position), t"$prefix$part.", next, true)
          case ':' => resolve(path.after(position), t"$prefix$part⌗", next, false)
          case _   => (if prefix.empty then t"" else prefix.keep(1, Rtl), part, current)

      . or:
         (if prefix.empty then t"" else prefix.keep(1, Rtl),
          path.urlDecode,
          current(if term then Member.OfTerm(path.urlDecode) else Member.OfType(path.urlDecode)))

  def apply(pkg: Text): Node = root(Member.OfTerm(pkg))

  def overlay(base: Amox.Base)(using Stdio): Unit =
    def recur(prefix: Text, entries: List[Amox.Entry], current: Node): Unit =
      entries.map: entry =>
        val part = entry.name.skip(1)
        val next = entry.name.at(Prim) match
          case '.' => current(Member.OfTerm(part))
          case '#' => current(Member.OfType(part))
          case other   => Out.println(m"Unexpected: ${other.inspect}") yet Unset

        next.let: next =>
          next.memo = entry.memo.dare(Markdown.parseInline)
          next.detail = entry.detail
          next.hidden = entry.hidden.or(false)
          recur(prefix+entry.name, entry.entry, next)

    val init = root(Member.OfTerm(base.base.or(t"")))
    init.memo = base.memo.dare(Markdown.parseInline)
    init.detail = base.detail
    recur(base.base.or(t""), base.entry, init)

  def load(path: Path on Linux)(using Stdio): Unit =
    val inspector = DocInspector()
    try TastyInspector.inspectTastyFilesInJar(path.encode.s)(inspector)
    catch case error: Throwable =>
      Out.println(error.stackTrace.teletype)

    Syntax.cache.clear()
    java.lang.System.gc()

  import Member.{OfTerm, OfType}

  case class DocInspector()(using Stdio) extends Inspector:
    def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
      import quotes.reflect.*
      import Flags.*

      def walk(ast: Tree, node: Node, ofTerm: Boolean): Unit =
        def of(name: Text): Member = if ofTerm then OfTerm(name) else OfType(name)
        ast match
          case pc@PackageClause(id@Ident(name), body) =>
            val child = node(of(name))
            child.definition = `package`(Nil)
            body.each(walk(_, child, true))

          case valDef@ValDef(name, rtn, body) if !(valDef.symbol.flags.is(Private) || name == "_") =>
            val flags = valDef.symbol.flags
            val termName =
              if flags.is(Given) && (name.tt.starts(t"evidence$$"))
              then name.tt/*Syntax(rtn.tpe)*/ else name.tt
            if name.tt.ends(t"$$package") then body.each(walk(_, node, true))
            else
              val child = node(of(termName))
              child.definition =
                if flags.is(Given)
                then `given`(flags.has(`inline`, `transparent`, `erased`))
                else if flags.is(Enum) && flags.is(Case) then `enum.case`(Nil)
                else if flags.is(Module) && flags.is(Case) then `case object`(Nil)
                else if flags.is(Module) then `object`(Nil)
                else if flags.is(Mutable)
                then `var`(flags.has(`override`, `private`, `protected`, `final`))
                else `val`(flags.has(`override`, `private`, `protected`, `erased`, `inline`, `final`, `lazy`))

              if !flags.is(Module) then child.returnType = Syntax(rtn.tpe)
              body.each(walk(_, child, true))

          case classDef@ClassDef(name, defDef, extensions0, selfType, body) =>
            val typeRef = classDef.symbol.typeRef
            val flags = classDef.symbol.flags
            val parents = typeRef.baseClasses.map(typeRef.baseType(_))

            given TypeRepr is PartiallyOrdered = (left, right) =>
              !(left =:= right) && left <:< right

            given TypeRepr is Showable = Syntax(_).show
            import dagStyles.default
            val caseClass = flags.is(Case)
            val dag = Poset(parents*).dag

            val extensions: List[Syntax] =
              dag(parents.head).filter: repr =>
                repr.typeSymbol != defn.ObjectClass
                && repr.typeSymbol != defn.AnyClass
                && repr.typeSymbol != defn.AnyRefClass
                && (caseClass && repr.typeSymbol != defn.ProductClass)
                && (caseClass && repr.typeSymbol != TypeRepr.of[java.io.Serializable].typeSymbol)

              . to(List)
              . map(Syntax(_))

            val obj = flags.is(Module)
            if name.tt.ends(t"$$package") || name.tt.ends(t"$$package$$")
            then body.each(walk(_, node, obj))
            else
              val className = if obj && name.tt.ends(t"$$") then name.tt.skip(1, Rtl) else name.tt
              val child = node(of(className))

              if obj && !(flags.is(Case) && flags.is(Enum))
              then () //child.definition = Definition.Object(flags.has(`private`, `case`))
              else if flags.is(Trait)
              then child.template = `trait`
                                     (flags.has(`private`, `erased`, `into`, `sealed`, `transparent`),
                                      extensions)
              else if flags.is(Enum)
              then
                child.template =
                  if flags.is(Case) then `case`(flags.has(`private`))
                  else `enum`(flags.has(`private`))
              else child.template =
                if flags.is(Case)
                then `case class`(flags.has(`private`, `protected`, `sealed`, `open`, `transparent`, `final`, `into`, `erased`, `abstract`), extensions)
                else `class`(flags.has(`private`, `protected`, `sealed`, `open`, `transparent`, `final`, `into`, `erased`, `abstract`), extensions)

              body.each(walk(_, child, obj))

          case term@DefDef(name, groups0, rtn, body) if !term.symbol.flags.is(Synthetic) && !term.symbol.flags.is(Private) && !name.contains("$default$") =>
            val flags = term.symbol.flags
            val isGiven = flags.is(Given)
            val termName = name.show
            val child = node(of(termName))
            val ext = flags.is(ExtensionMethod)

            val split = 1 + groups0.indexWhere:
              case clause@TermParamClause(terms) => terms.length == 1
                                                    && !terms.exists(_.symbol.flags.is(Given))
              case _                             => false

            val preClauses = if ext then groups0.take(split) else Nil
            val paramClauses = if ext then groups0.drop(split) else groups0

            child.params = Syntax(10, paramClauses.map(Syntax.clause(_))*)
            child.definition =
              if isGiven then `given`(flags.has(`inline`, `transparent`, `erased`))
              else
                val definition: amok.Definition.`def` =
                  `def`(flags.has(`abstract`, `override`, `private`, `protected`, `erased`, `final`, `infix`, `transparent`, `inline`))

                if ext then `extension`(Syntax(10, preClauses.map(Syntax.clause(_))*), definition)
                else definition

            child.returnType = Syntax(rtn.tpe)

          case typeDef@TypeDef(name, _) if name != "MirroredMonoType" =>
            val flags = typeDef.symbol.flags
            val typeName = name.show
            val child = node(of(typeName))
            child.template = `type`(flags.has(`into`, `opaque`, `infix`))

          case Export(x, exports) => exports.map:
            case SimpleSelector(name)    => node(of(name.tt))
            case RenameSelector(_, name) => node(of(name.tt))
            case other: Selector         => Out.println(t"Found a different kind of selector")

          case other =>
            //Out.println(t"OTHER: ${other.toString}")

      tastys.each: tasty =>
        walk(tasty.ast, root, true)

enum Visibility:
  case Private, Protected, Public

extension (using Quotes)(flags: quotes.reflect.Flags)
  def modifier(modifier: Modifier): Boolean =
    import quotes.reflect.*
    modifier match
      case `private`     => flags.is(Flags.Private)
      case `protected`   => flags.is(Flags.Protected)
      case `abstract`    => flags.is(Flags.Abstract) || flags.is(Flags.AbsOverride)
      case `open`        => flags.is(Flags.Open)
      case `final`       => flags.is(Flags.Final)
      case `erased`      => flags.is(Flags.Erased)
      case `transparent` => flags.is(Flags.Transparent)
      case `inline`      => flags.is(Flags.Inline)
      case `lazy`        => flags.is(Flags.Lazy)
      case `sealed`      => flags.is(Flags.Sealed)
      case `override`    => flags.is(Flags.Override) || flags.is(Flags.AbsOverride)
      case `opaque`      => flags.is(Flags.Opaque)
      case `infix`       => flags.is(Flags.Infix)
      case `into`        => false
      case `tracked`     => false

  def has(modifiers: Modifier*): List[Modifier] = modifiers.filter(flags.modifier(_)).to(List)

object Modifier:
  val all: List[Modifier] = values.to(List)

enum Modifier:
  case `private`, `abstract`, `open`, `final`, `erased`, `transparent`, `inline`, `lazy`, `sealed`,
      `override`, `opaque`, `infix`, `into`, `tracked`, `protected`

  def keyword: Syntax = Syntax.Symbolic(this.toString.tt.lower)

export Modifier.*

enum Definition:
  case `package`(modifiers: List[Modifier])
  case `object`(modifiers: List[Modifier])
  case `case object`(modifiers: List[Modifier])
  case `enum.case`(modifiers: List[Modifier])
  case `def`(modifiers: List[Modifier])
  case `val`(modifiers: List[Modifier])
  case `var`(modifiers: List[Modifier])
  case `given`(modifiers: List[Modifier])
  case `extension`(params: Syntax, `def`: Definition.`def`, modifiers: List[Modifier] = Nil)

  def keyword: Syntax = Syntax.Symbolic:
    this match
      case _: `package`            => t"package"
      case _: `object`             => t"object"
      case _: `case object`        => t"case object"
      case _: `enum.case`          => t"case"
      case _: `def`                => t"def"
      case _: `val`                => t"val"
      case _: `var`                => t"var"
      case _: `given`              => t"given"
      case _: `extension`          => t"extension"

  def modifiers: List[Modifier]

  def syntax: Syntax = this match
    case `extension`(param, definition, _) =>
      Syntax(0, Syntax.Symbolic(t"extension"), Syntax.Space, param, Syntax.Space, definition.syntax)
    case other =>
      Syntax.sequence(other.modifiers.map(_.keyword), Syntax.Space)
      . let(Syntax(0, _, Syntax.Space, keyword))
      . or(keyword)

export Definition.*

enum Template:
  case `case class`(modifiers: List[Modifier], extensions: List[Syntax] = Nil, derivations: List[Text] = Nil)
  case `class`(modifiers: List[Modifier], extensions: List[Syntax] = Nil, derivations: List[Text] = Nil)
  case `trait`(modifiers: List[Modifier], extensions: List[Syntax] = Nil)
  case `enum`(modifiers: List[Modifier], extensions: List[Syntax] = Nil, derivations: List[Text] = Nil)
  case `case`(modifiers: List[Modifier], extensions: List[Syntax] = Nil)
  case `type`(modifiers: List[Modifier], extensions: Nil.type = Nil)

  def extensions: List[Syntax]

  def keyword: Syntax.Symbolic = Syntax.Symbolic:
    this match
      case _: `case class` => t"case class"
      case _: `class`      => t"class"
      case _: `trait`      => t"trait"
      case _: `enum`       => t"enum"
      case _: `case`       => t"case"
      case _: `type`       => t"type"

  def modifiers: List[Modifier]

  def syntax: Syntax =
    Syntax.sequence(modifiers.map(_.keyword), Syntax.Space).let(Syntax(0, _, Syntax.Space, keyword))
    . or(keyword)

export Template.*
