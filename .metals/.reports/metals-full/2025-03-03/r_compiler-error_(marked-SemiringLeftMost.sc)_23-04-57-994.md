file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/.ammonite/scala-2.13.14/amm-3.0.0-2-6342755f/marked-SemiringLeftMost/src/ammonite/$file/marked-SemiringLeftMost.amm.sc.scala
### java.lang.IndexOutOfBoundsException: 0 is out of bounds (min 0, max -1)

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 2780
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
      case BCHAR(f@@) => REGW(semiring.zero, semiring.times(mark, f(c)), CHARw(f)) 
      case ALTw(r1, r2) => altw(shift(mark, r1.re, c), shift(mark, r2.re, c))
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
scala.collection.mutable.ArrayBuffer.apply(ArrayBuffer.scala:103)
	dotty.tools.dotc.core.Contexts$.comparer(Contexts.scala:830)
	dotty.tools.dotc.core.Contexts$.inline$comparer(Contexts.scala:826)
	dotty.tools.dotc.core.TypeComparer$.matchesType(TypeComparer.scala:3020)
	dotty.tools.dotc.core.Types$Type.matches(Types.scala:1157)
	dotty.tools.dotc.core.Types$Type.matchesLoosely(Types.scala:1164)
	dotty.tools.dotc.core.Denotations$Denotation.qualifies$1(Denotations.scala:358)
	dotty.tools.dotc.core.Denotations$Denotation.matchingDenotation(Denotations.scala:364)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.matchingDecl(SymDenotations.scala:1352)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.overriddenSymbol(SymDenotations.scala:1380)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.overriddenFromType$$anonfun$1(SymDenotations.scala:1403)
	scala.collection.Iterator$$anon$9.next(Iterator.scala:584)
	scala.collection.Iterator$$anon$6.hasNext(Iterator.scala:478)
	scala.collection.IterableOnceOps.exists(IterableOnce.scala:647)
	scala.collection.IterableOnceOps.exists$(IterableOnce.scala:644)
	scala.collection.AbstractIterator.exists(Iterator.scala:1303)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.hasDefaultParams(SymDenotations.scala:969)
	dotty.tools.dotc.typer.Namer$ClassCompleter.addForwarder$1(Namer.scala:1194)
	dotty.tools.dotc.typer.Namer$ClassCompleter.addWildcardForwardersNamed$1$$anonfun$2(Namer.scala:1321)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:334)
	dotty.tools.dotc.typer.Namer$ClassCompleter.addWildcardForwardersNamed$1(Namer.scala:1321)
	dotty.tools.dotc.typer.Namer$ClassCompleter.addWildcardForwarders$1$$anonfun$1(Namer.scala:1344)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:334)
	dotty.tools.dotc.typer.Namer$ClassCompleter.addWildcardForwarders$1(Namer.scala:1328)
	dotty.tools.dotc.typer.Namer$ClassCompleter.addForwarders$1(Namer.scala:1349)
	dotty.tools.dotc.typer.Namer$ClassCompleter.exportForwarders(Namer.scala:1412)
	dotty.tools.dotc.typer.Namer$ClassCompleter.processExport$1(Namer.scala:1422)
	dotty.tools.dotc.typer.Namer$ClassCompleter.process$1(Namer.scala:1444)
	dotty.tools.dotc.typer.Namer$ClassCompleter.processExports(Namer.scala:1465)
	dotty.tools.dotc.typer.Namer$ClassCompleter.completeInCreationContext(Namer.scala:1666)
	dotty.tools.dotc.typer.Namer$Completer.complete(Namer.scala:828)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.completeFrom(SymDenotations.scala:174)
	dotty.tools.dotc.core.Denotations$Denotation.completeInfo$1(Denotations.scala:188)
	dotty.tools.dotc.core.Denotations$Denotation.info(Denotations.scala:190)
	dotty.tools.dotc.core.Types$NamedType.info(Types.scala:2354)
	dotty.tools.dotc.core.Types$Type.isRef(Types.scala:198)
	dotty.tools.dotc.core.TypeComparer.isBottom(TypeComparer.scala:114)
	dotty.tools.dotc.core.TypeComparer.secondTry$1(TypeComparer.scala:418)
	dotty.tools.dotc.core.TypeComparer.firstTry$1(TypeComparer.scala:408)
	dotty.tools.dotc.core.TypeComparer.recur(TypeComparer.scala:1472)
	dotty.tools.dotc.core.TypeComparer.isSubType(TypeComparer.scala:206)
	dotty.tools.dotc.core.TypeComparer.isSubType(TypeComparer.scala:216)
	dotty.tools.dotc.core.TypeComparer.topLevelSubType(TypeComparer.scala:126)
	dotty.tools.dotc.core.TypeComparer$.topLevelSubType(TypeComparer.scala:2996)
	dotty.tools.dotc.core.Types$Type.$less$colon$less(Types.scala:1078)
	dotty.tools.dotc.core.Types$Type.relaxed_$less$colon$less(Types.scala:1110)
	dotty.tools.dotc.typer.ProtoTypes$Compatibility.isCompatible(ProtoTypes.scala:42)
	dotty.tools.dotc.typer.ProtoTypes$Compatibility.isCompatible$(ProtoTypes.scala:29)
	dotty.tools.dotc.typer.ProtoTypes$NoViewsAllowed$.isCompatible(ProtoTypes.scala:123)
	dotty.tools.dotc.typer.Implicits$ImplicitRefs.candidateKind$1(Implicits.scala:239)
	dotty.tools.dotc.typer.Implicits$ImplicitRefs.tryCandidate$1(Implicits.scala:253)
	dotty.tools.dotc.typer.Implicits$ImplicitRefs.filterMatching$$anonfun$2(Implicits.scala:262)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:334)
	dotty.tools.dotc.typer.Implicits$ImplicitRefs.filterMatching(Implicits.scala:262)
	dotty.tools.dotc.typer.Implicits$ContextualImplicits.computeEligible(Implicits.scala:367)
	dotty.tools.dotc.typer.Implicits$ContextualImplicits.eligible(Implicits.scala:359)
	dotty.tools.dotc.typer.Implicits$ContextualImplicits.computeEligible(Implicits.scala:369)
	dotty.tools.dotc.typer.Implicits$ContextualImplicits.eligible(Implicits.scala:359)
	dotty.tools.dotc.typer.Implicits$ContextualImplicits.computeEligible(Implicits.scala:369)
	dotty.tools.dotc.typer.Implicits$ContextualImplicits.eligible(Implicits.scala:359)
	dotty.tools.dotc.typer.Implicits$ImplicitSearch.searchImplicit(Implicits.scala:1545)
	dotty.tools.dotc.typer.Implicits$ImplicitSearch.bestImplicit(Implicits.scala:1580)
	dotty.tools.dotc.typer.Implicits.inferImplicit(Implicits.scala:1067)
	dotty.tools.dotc.typer.Implicits.inferImplicit$(Implicits.scala:820)
	dotty.tools.dotc.typer.Typer.inferImplicit(Typer.scala:116)
	dotty.tools.dotc.typer.Implicits.inferImplicitArg(Implicits.scala:886)
	dotty.tools.dotc.typer.Implicits.inferImplicitArg$(Implicits.scala:820)
	dotty.tools.dotc.typer.Typer.inferImplicitArg(Typer.scala:116)
	dotty.tools.dotc.typer.Typer.implicitArgs$1(Typer.scala:3820)
	dotty.tools.dotc.typer.Typer.addImplicitArgs$1(Typer.scala:3892)
	dotty.tools.dotc.typer.Typer.adaptNoArgsImplicitMethod$1(Typer.scala:3945)
	dotty.tools.dotc.typer.Typer.adaptNoArgs$1(Typer.scala:4142)
	dotty.tools.dotc.typer.Typer.adapt1(Typer.scala:4393)
	dotty.tools.dotc.typer.Typer.adapt(Typer.scala:3669)
	dotty.tools.dotc.typer.ProtoTypes$FunProto.typedArg(ProtoTypes.scala:500)
	dotty.tools.dotc.typer.Applications$ApplyToUntyped.typedArg(Applications.scala:925)
	dotty.tools.dotc.typer.Applications$ApplyToUntyped.typedArg(Applications.scala:925)
	dotty.tools.dotc.typer.Applications$Application.addTyped$1(Applications.scala:617)
	dotty.tools.dotc.typer.Applications$Application.matchArgs(Applications.scala:681)
	dotty.tools.dotc.typer.Applications$Application.init(Applications.scala:520)
	dotty.tools.dotc.typer.Applications$TypedApply.<init>(Applications.scala:807)
	dotty.tools.dotc.typer.Applications$ApplyToUntyped.<init>(Applications.scala:924)
	dotty.tools.dotc.typer.Applications.ApplyTo(Applications.scala:1186)
	dotty.tools.dotc.typer.Applications.ApplyTo$(Applications.scala:380)
	dotty.tools.dotc.typer.Typer.ApplyTo(Typer.scala:116)
	dotty.tools.dotc.typer.Applications.simpleApply$1(Applications.scala:997)
	dotty.tools.dotc.typer.Applications.realApply$1$$anonfun$2(Applications.scala:1107)
	dotty.tools.dotc.typer.Typer.tryEither(Typer.scala:3406)
	dotty.tools.dotc.typer.Applications.realApply$1(Applications.scala:1122)
	dotty.tools.dotc.typer.Applications.typedApply(Applications.scala:1160)
	dotty.tools.dotc.typer.Applications.typedApply$(Applications.scala:380)
	dotty.tools.dotc.typer.Typer.typedApply(Typer.scala:116)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3116)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3382)
	dotty.tools.dotc.typer.Typer.typedBlock(Typer.scala:1232)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3124)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3382)
	dotty.tools.dotc.typer.Typer.caseRest$1(Typer.scala:1941)
	dotty.tools.dotc.typer.Typer.typedCase(Typer.scala:1957)
	dotty.tools.dotc.typer.Typer.typedCases$$anonfun$1(Typer.scala:1885)
	dotty.tools.dotc.core.Decorators$.loop$1(Decorators.scala:94)
	dotty.tools.dotc.core.Decorators$.mapconserve(Decorators.scala:110)
	dotty.tools.dotc.typer.Typer.typedCases(Typer.scala:1884)
	dotty.tools.dotc.typer.Typer.$anonfun$34(Typer.scala:1877)
	dotty.tools.dotc.typer.Applications.harmonic(Applications.scala:2418)
	dotty.tools.dotc.typer.Applications.harmonic$(Applications.scala:380)
	dotty.tools.dotc.typer.Typer.harmonic(Typer.scala:116)
	dotty.tools.dotc.typer.Typer.typedMatchFinish(Typer.scala:1877)
	dotty.tools.dotc.typer.Typer.typedMatch(Typer.scala:1811)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3130)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3382)
	dotty.tools.dotc.typer.Typer.$anonfun$57(Typer.scala:2553)
	dotty.tools.dotc.inlines.PrepareInlineable$.dropInlineIfError(PrepareInlineable.scala:242)
	dotty.tools.dotc.typer.Typer.typedDefDef(Typer.scala:2553)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3092)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3196)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3293)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3339)
	dotty.tools.dotc.typer.Typer.typedClassDef(Typer.scala:2736)
	dotty.tools.dotc.typer.Typer.typedTypeOrClassDef$1(Typer.scala:3104)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3108)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3196)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3293)
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
	dotty.tools.pc.HoverProvider$.hover(HoverProvider.scala:40)
	dotty.tools.pc.ScalaPresentationCompiler.hover$$anonfun$1(ScalaPresentationCompiler.scala:376)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: 0 is out of bounds (min 0, max -1)