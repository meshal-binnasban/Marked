file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/.ammonite/scala-2.13.14/amm-3.0.0-2-6342755f/marked-SemiringLeftMost/src/ammonite/$file/marked-SemiringLeftMost.amm.sc.scala
### java.lang.AssertionError: assertion failed: denotation class WithPureFuns invalid in run 3. ValidFor: Period(4.1-5)

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 2873
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/.ammonite/scala-2.13.14/amm-3.0.0-2-6342755f/marked-SemiringLeftMost/src/ammonite/$file/marked-SemiringLeftMost.amm.sc.scala
text:
```scala

package ammonite
package $file
import _root_.ammonite.interp.api.InterpBridge.{
  value => interp
}
import _root_.ammonite.interp.api.InterpBridge.value.{
  exit,
  scalaVersion
}
import _root_.ammonite.interp.api.IvyConstructor.{
  ArtifactIdExt,
  GroupIdExt
}
import _root_.ammonite.compiler.CompilerExtensions.{
  CompilerInterpAPIExtensions,
  CompilerReplAPIExtensions
}
import _root_.ammonite.runtime.tools.{
  browse,
  grep,
  time,
  tail
}
import _root_.ammonite.compiler.tools.{
  desugar,
  source
}
import _root_.mainargs.{
  arg,
  main
}
import _root_.ammonite.repl.tools.Util.{
  PathRead
}
import _root_.ammonite.repl.ReplBridge.value.{
  codeColorsImplicit
}


object `marked-SemiringLeftMost`{
/*<start>*/
import scala.compiletime.ops.boolean
import scala.math.min
import scala.language.implicitConversions

enum Rexp {
  case ZERO
  case ONE 
  case CHAR(c: Char)
  case ALT(r1: Rexp, r2: Rexp )
  case SEQ(r1: Rexp, r2: Rexp)
  case STAR(r: Rexp)
//  case NTIMES(r: Rexp, n: Int) 
}
import Rexp._
def OPT(r: Rexp) = ALT(r, ONE)

//BFUN(f: Char => Boolean)
//def BCSET(cs Set[Char]): Boolean= if(cs.contains(c)) semiring.one else semiring.zero
//def CHAR(c:Char): Boolean= x => if (x == c) semiring.one else semiring.zero
//def ALL(): Boolean= _>true

/*
case class REGW[C, S](emptyw: S, finalw: S, re: REw[C, S])
sealed trait REw[C, S]
case class ZEROw[C, S]() extends REw[C, S]
case class ONEw[C, S]() extends REw[C, S]
case class CHARw[C, S](f: C => S) extends REw[C, S]
case class ALTw[C, S](a: REw[C, S], b: REw[C, S]) extends REw[C, S]
case class SEQw[C, S](a: REw[C, S], b: REw[C, S]) extends REw[C, S]
case class STARw[C, S](a: REw[C, S]) extends REw[C, S]

*/

enum REG[S] {
  case BZERO()
  case BONE() 
  case BCHAR(b: S, c: Char)
  case BALT(r1: REG[S], r2: REG[S])
  case BSEQ(r1: REG[S], r2: REG[S])
  case BSTAR(r: REG[S])
  case BINIT(r: REG[S])
}
import REG._

def nullable[S](r: REG[S])(using semiring: SemiringI[S]) : S = r match {
  case BZERO() => semiring.zero
  case BONE()=> semiring.one
  case BCHAR(b,c) =>  semiring.zero
  case BALT(r1, r2) => semiring.plus(nullable(r1),nullable(r2))
  case BSEQ(r1, r2) => semiring.times(nullable(r1),nullable(r2))
  case BSTAR(r) => semiring.one
  case BINIT(r) => nullable(r)
}

def fin[S](r: REG[S])(using semiring: SemiringI[S]) : S = r match {
  case BZERO() => semiring.zero
  case BONE() => semiring.zero
  case BCHAR(b,_) => b
  case BALT(r1, r2) => semiring.plus( fin(r1) , fin(r2) )
  case BSEQ(r1, r2) => semiring.plus( 
    semiring.times( fin(r1) , nullable(r2) ) ,  fin(r2)  )
  case BSTAR(r) => fin(r)
}

def shift[S](mark: S, re: REG[S], c: Char)(using semiring: SemiringI[S]): REG[S] = {
    re match {
      case BZERO() => BZERO()
      case BONE() => BONE()
      case BCHAR(b,c) => REGW(semiring.zero, semiring.times(mark, f(c)), CHARw(f)) 
      case ALTw(r1, r2) => @@altw(shift(mark, r1.re, c), shift(mark, r2.re, c))
      case SEQw(r1, r2) => seqw(shift(mark, r1.re, c),
        shift(semiring.plus( semiring.times(mark, r1.emptyw), 
                                    r1.finalw), r2.re, c))
      case STARw(r) => starw(shift(semiring.plus(mark, r.finalw), r.re, c))
    }
  }


def zerow[C, S](using semiring: SemiringI[S]): REGW[C, S] = {
  REGW(semiring.zero, semiring.zero, ZEROw()) }
//Smart Constructors page 7 -  how to calculate final and empty From the Article
def onew[C, S](using semiring: SemiringI[S]): REGW[C, S] = {
  REGW(semiring.one, semiring.zero, ONEw()) }

def charw[C, S](f: C => S)(using semiring: SemiringI[S]): REGW[C, S] = {
  REGW(semiring.zero, semiring.zero, CHARw(f)) }

def chari[C,S](ch:Char)(using semiring: SemiringI[S]): REGW[C, S] = {
    def weight[C](t: C): S = {
        t match {
            case (pos: Int, x: Char) => if (x == ch) semiring.index(pos) else semiring.zero
            case _ =>semiring.zero 
        }
    }
    charw(weight)
}
def altw[C, S](r1: REGW[C, S], r2: REGW[C, S])(using semiring: SemiringI[S]): REGW[C, S] = {
  REGW(semiring.plus(r1.emptyw,r2.emptyw) 
        , semiring.plus(r1.finalw,r2.finalw)
          , ALTw(r1,r2))
}
def seqw[C, S](r1: REGW[C, S], r2: REGW[C, S])(using semiring: SemiringI[S]): REGW[C, S] = {
  REGW(semiring.times(r1.emptyw, r2.emptyw) 
        , semiring.plus(semiring.times(r1.finalw, r2.emptyw), r2.finalw)
          , SEQw(r1,r2))
}
def starw[C, S](r1: REGW[C, S])(using semiring: SemiringI[S]): REGW[C, S] = {
  REGW(semiring.one, r1.finalw , STARw(r1))  }

trait Semiring[S] {
  def zero: S   // Additive identity
  def one: S    // Multiplicative identity
  def plus(a: S, b: S): S  // ⊕ Addition
  def times(a: S, b: S): S // ⊗ Multiplication
}

trait SemiringI[S] extends Semiring[S] {
    def index(i: Int): S
  }

sealed trait StartT
case object NoStart extends StartT
case class Start(pos: Int) extends StartT
sealed trait LeftmostT
case object NoLeft extends LeftmostT
case class Leftmost(s: StartT) extends LeftmostT

given semiringLeftmost: Semiring[LeftmostT] with {
  def zero =  NoLeft 
  def one = Leftmost(NoStart) 

  def plus(a: LeftmostT, b: LeftmostT): LeftmostT = (a,b) match{
    case (NoLeft, x) => x
    case (x, NoLeft) => x
    case (Leftmost(i), Leftmost(j)) => Leftmost(leftmost(i, j))
  } 
  def times(a: LeftmostT, b: LeftmostT): LeftmostT = (a,b) match {
        case (NoLeft, _) => NoLeft
        case (_, NoLeft) => NoLeft
        case (Leftmost(x), Leftmost(y)) =>  Leftmost(start(x, y))
  }
      
  def start(i: StartT,j:StartT): StartT = (i,j) match {
        case (NoStart, s) => s
        case (s, _) => s
    }  

  def leftmost(i: StartT,j:StartT): StartT = (i,j) match {
        case (NoStart, NoStart) => NoStart
        case (NoStart, Start(i)) => Start(i)
        case (Start(i), NoStart) => Start(i)
        case (Start(i), Start(j)) => Start(min(i, j))
    }
}
given semiringILeftmost: SemiringI[LeftmostT] with {
    export semiringLeftmost.*// Inherit all `Semiring` operations
    def index(i: Int): LeftmostT = Leftmost(Start(i))

}

def matcher[C, S](r: REGW[C, S], s: List[C])(using semiring: SemiringI[S]): S = {
    s match {
      case Nil => r.emptyw
      case c :: cs => val  x =cs.foldLeft(shift(semiring.one, r.re, c))
        ((r, c) => shift(semiring.zero, r.re, c))
        x.finalw
    }
}


def submatcher[C, S](r: REGW[C, S], s: List[C])(using semiring: SemiringI[S]): S = {
    val arb: REGW[C, S] = starw(charw(_ => semiring.one))
    matcher(seqw(arb, seqw(r, arb)), (s.indices zip s).toList.asInstanceOf[List[C]])  
}



def weighted[S](r: Rexp)(using semiring: SemiringI[S]): REGW[Char,S] = r match {
  case ZERO      => zerow//[C, S]
  case ONE       => onew//[C, S]
  case CHAR(c)   => chari(c)(using semiring)
  case ALT(r1, r2)  => altw(weighted(r1), weighted(r2))
  case SEQ(r1, r2)  => seqw(weighted(r1), weighted(r2))
  case STAR(r1)     => starw(weighted(r1))
}

@main
def test0() = {

val a = weighted(CHAR('a')) 
val aStar = starw(a)

val b = weighted(CHAR('b')) 
val bStar=starw(b)

val r= seqw(aStar,b)
val s ="hello athis isabb test of left most with ab"
println(submatcher(r, s.toList))

//val r2 = weighted ( STAR(ALT(CHAR('a'), CHAR('b'))))(using semiringILeftmost)
//println(submatcher(r2, "hello ab test abab".toList)(using semiringILeftmost))

}

@main
def test1() = {
    //booleanSemiring
    //semiringILeftmost
    //semiringILeftlong
   
   // val aStarB: Rexp = "a".% ~"b"
   // println(string(aStarB))
   // val reg=weighted(aStarB) (using  semiringILeftlong)
    //val s="aab aaab b"
    //println(submatcher(reg,s.toList))

  
   val x= CHAR('x')
   val y= CHAR('y')
   val xpy= x | y
   val xy=x~y
   val reg= weighted(STAR(xpy | xy))
   val s="xy"
   submatcher(reg,s.toList)

}

/* no NTIMES/OPT Constructors so far
def EVIL1(n: Int) = 
  SEQ(NTIMES(OPT(CHAR('a')), n), NTIMES(CHAR('a'), n))
*/

val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

@main
def test2() = {
  /*
  //no NTIMES/OPT Constructors so far
  for (i <- 0 to 8000 by 1000) {
    println(f"$i: ${time_needed(2, matcher(weighted(EVIL1(i))(using booleanSemiring), ("a" * i).toList))}%.5f")
  }
  */
}

//@arg(doc = "Test (a*)* b")
@main
def test3() = {
  for (i <- 0 to 7000000 by 500000) {
    println(f"$i: ${time_needed(2, matcher(weighted(EVIL2), ("a" * i).toList))}%.5f")
  }
} 

//@arg(doc = "All tests.")
@main
def all() = { test2(); test3() } 

def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

// strings are coerced into Rexps
given Conversion[String, Rexp] = s => charlist2rexp(s.toList)
//val ABCD : Rexp = "abcd"

extension (r: Rexp) {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

def string(r: Rexp) : String = r match {
  case ZERO => "0"
  case ONE => "1"
  case CHAR(c) => c.toString 
  case ALT(r1, r2) => s"(${string(r1)} + ${string(r2)})"
  case SEQ(CHAR(c), CHAR(d)) => s"${c}${d}"
  case SEQ(r1, r2) => s"(${string(r1)} ~ ${string(r2)})"
  case STAR(r) => s"(${string(r)})*"
} 
}

```



#### Error stacktrace:

```
scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:8)
	dotty.tools.dotc.core.Denotations$SingleDenotation.updateValidity(Denotations.scala:718)
	dotty.tools.dotc.core.Denotations$SingleDenotation.bringForward(Denotations.scala:743)
	dotty.tools.dotc.core.Denotations$SingleDenotation.toNewRun$1(Denotations.scala:800)
	dotty.tools.dotc.core.Denotations$SingleDenotation.current(Denotations.scala:871)
	dotty.tools.dotc.core.Symbols$Symbol.recomputeDenot(Symbols.scala:117)
	dotty.tools.dotc.core.Symbols$Symbol.computeDenot(Symbols.scala:111)
	dotty.tools.dotc.core.Symbols$Symbol.denot(Symbols.scala:104)
	dotty.tools.dotc.core.Symbols$Symbol.name(Symbols.scala:257)
	dotty.tools.dotc.core.tasty.TreeUnpickler$$anon$4.hasSymbol(TreeUnpickler.scala:753)
	dotty.tools.dotc.core.tasty.TreeUnpickler.dotty$tools$dotc$core$tasty$TreeUnpickler$TreeReader$$_$createMemberSymbol$$anonfun$1(TreeUnpickler.scala:652)
	scala.collection.immutable.List.exists(List.scala:396)
	dotty.tools.dotc.core.tasty.TreeUnpickler$TreeReader.createMemberSymbol(TreeUnpickler.scala:652)
	dotty.tools.dotc.core.tasty.TreeUnpickler$TreeReader.createSymbol(TreeUnpickler.scala:563)
	dotty.tools.dotc.core.tasty.TreeUnpickler$TreeReader.symbolAtCurrent(TreeUnpickler.scala:285)
	dotty.tools.dotc.core.tasty.TreeUnpickler$TreeReader.indexStats(TreeUnpickler.scala:767)
	dotty.tools.dotc.core.tasty.TreeUnpickler$TreeReader.indexStats$$anonfun$1$$anonfun$1(TreeUnpickler.scala:777)
	dotty.tools.dotc.core.tasty.TreeUnpickler$TreeReader.processPackage(TreeUnpickler.scala:797)
	dotty.tools.dotc.core.tasty.TreeUnpickler$TreeReader.processPackage(TreeUnpickler.scala:793)
	dotty.tools.dotc.core.tasty.TreeUnpickler$TreeReader.indexStats(TreeUnpickler.scala:777)
	dotty.tools.dotc.core.tasty.TreeUnpickler.enter(TreeUnpickler.scala:115)
	dotty.tools.dotc.core.tasty.DottyUnpickler.enter(DottyUnpickler.scala:57)
	dotty.tools.dotc.core.classfile.ClassfileParser.unpickleTASTY$1(ClassfileParser.scala:952)
	dotty.tools.dotc.core.classfile.ClassfileParser.unpickleOrParseInnerClasses(ClassfileParser.scala:1020)
	dotty.tools.dotc.core.classfile.ClassfileParser.parseClass(ClassfileParser.scala:218)
	dotty.tools.dotc.core.classfile.ClassfileParser.$anonfun$1(ClassfileParser.scala:127)
	dotty.tools.dotc.core.classfile.ClassfileParser.run(ClassfileParser.scala:122)
	dotty.tools.dotc.core.ClassfileLoader.load(SymbolLoaders.scala:412)
	dotty.tools.dotc.core.ClassfileLoader.doComplete(SymbolLoaders.scala:407)
	dotty.tools.dotc.core.SymbolLoader$$anon$1.doComplete(SymbolLoaders.scala:325)
	dotty.tools.dotc.core.SymbolLoader.complete(SymbolLoaders.scala:341)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.completeFrom(SymDenotations.scala:174)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.completeOnce(SymDenotations.scala:384)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.isAbsent(SymDenotations.scala:614)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.isAccessibleFrom(SymDenotations.scala:916)
	dotty.tools.dotc.typer.ProtoTypes$SelectionProto.qualifies$1(ProtoTypes.scala:209)
	dotty.tools.dotc.typer.ProtoTypes$SelectionProto.isMatchedBy(ProtoTypes.scala:216)
	dotty.tools.dotc.core.TypeComparer.isMatchedByProto(TypeComparer.scala:2101)
	dotty.tools.dotc.core.TypeComparer.firstTry$1(TypeComparer.scala:337)
	dotty.tools.dotc.core.TypeComparer.recur(TypeComparer.scala:1472)
	dotty.tools.dotc.core.TypeComparer.isSubType(TypeComparer.scala:206)
	dotty.tools.dotc.core.TypeComparer.isSubType(TypeComparer.scala:216)
	dotty.tools.dotc.core.TypeComparer.topLevelSubType(TypeComparer.scala:126)
	dotty.tools.dotc.core.TypeComparer.testSubType(TypeComparer.scala:142)
	dotty.tools.dotc.core.TypeComparer$.testSubType(TypeComparer.scala:3011)
	dotty.tools.dotc.typer.Typer.adaptNoArgsOther$1(Typer.scala:4084)
	dotty.tools.dotc.typer.Typer.adaptNoArgs$1(Typer.scala:4166)
	dotty.tools.dotc.typer.Typer.adapt1(Typer.scala:4393)
	dotty.tools.dotc.typer.Typer.adapt(Typer.scala:3669)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3382)
	dotty.tools.dotc.typer.Typer.typeSelectOnTerm$1(Typer.scala:809)
	dotty.tools.dotc.typer.Typer.typedSelect(Typer.scala:847)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3085)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3196)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3382)
	dotty.tools.dotc.typer.Namer.typedAheadExpr$$anonfun$1(Namer.scala:1701)
	dotty.tools.dotc.typer.Namer.typedAhead(Namer.scala:1691)
	dotty.tools.dotc.typer.Namer.typedAheadExpr(Namer.scala:1701)
	dotty.tools.dotc.typer.Namer$Completer.$anonfun$15(Namer.scala:808)
	dotty.tools.dotc.typer.Typer.typedImportQualifier(Typer.scala:2851)
	dotty.tools.dotc.typer.Namer$Completer.typeSig(Namer.scala:808)
	dotty.tools.dotc.typer.Namer$Completer.completeInCreationContext(Namer.scala:952)
	dotty.tools.dotc.typer.Namer$Completer.complete(Namer.scala:828)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.completeFrom(SymDenotations.scala:174)
	dotty.tools.dotc.core.Denotations$Denotation.completeInfo$1(Denotations.scala:188)
	dotty.tools.dotc.core.Denotations$Denotation.info(Denotations.scala:190)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.ensureCompleted(SymDenotations.scala:392)
	dotty.tools.dotc.typer.Typer.retrieveSym(Typer.scala:3057)
	dotty.tools.dotc.typer.Typer.typedImport(Typer.scala:2854)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3128)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3283)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3339)
	dotty.tools.dotc.typer.Typer.typedPackageDef(Typer.scala:2879)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3149)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3320)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3339)
	dotty.tools.dotc.typer.Typer.typedPackageDef(Typer.scala:2879)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3149)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3382)
	dotty.tools.dotc.typer.TyperPhase.typeCheck$$anonfun$1(TyperPhase.scala:45)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	dotty.tools.dotc.core.Phases$Phase.monitor(Phases.scala:458)
	dotty.tools.dotc.typer.TyperPhase.typeCheck(TyperPhase.scala:51)
	dotty.tools.dotc.typer.TyperPhase.$anonfun$4(TyperPhase.scala:97)
	scala.collection.Iterator$$anon$6.hasNext(Iterator.scala:479)
	scala.collection.Iterator$$anon$9.hasNext(Iterator.scala:583)
	scala.collection.immutable.List.prependedAll(List.scala:152)
	scala.collection.immutable.List$.from(List.scala:685)
	scala.collection.immutable.List$.from(List.scala:682)
	scala.collection.IterableOps$WithFilter.map(Iterable.scala:900)
	dotty.tools.dotc.typer.TyperPhase.runOn(TyperPhase.scala:96)
	dotty.tools.dotc.Run.runPhases$1$$anonfun$1(Run.scala:315)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.ArrayOps$.foreach$extension(ArrayOps.scala:1323)
	dotty.tools.dotc.Run.runPhases$1(Run.scala:308)
	dotty.tools.dotc.Run.compileUnits$$anonfun$1(Run.scala:349)
	dotty.tools.dotc.Run.compileUnits$$anonfun$adapted$1(Run.scala:358)
	dotty.tools.dotc.util.Stats$.maybeMonitored(Stats.scala:69)
	dotty.tools.dotc.Run.compileUnits(Run.scala:358)
	dotty.tools.dotc.Run.compileSources(Run.scala:261)
	dotty.tools.dotc.interactive.InteractiveDriver.run(InteractiveDriver.scala:161)
	dotty.tools.pc.MetalsDriver.run(MetalsDriver.scala:45)
	dotty.tools.pc.PcDefinitionProvider.definitions(PcDefinitionProvider.scala:44)
	dotty.tools.pc.PcDefinitionProvider.definitions(PcDefinitionProvider.scala:33)
	dotty.tools.pc.ScalaPresentationCompiler.definition$$anonfun$1(ScalaPresentationCompiler.scala:156)
```
#### Short summary: 

java.lang.AssertionError: assertion failed: denotation class WithPureFuns invalid in run 3. ValidFor: Period(4.1-5)