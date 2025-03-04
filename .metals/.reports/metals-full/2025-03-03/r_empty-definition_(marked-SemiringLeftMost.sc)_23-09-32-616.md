error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-SemiringLeftMost.sc:66
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-SemiringLeftMost.sc
empty definition using pc, found symbol in pc: 
semanticdb not found
|empty definition using fallback
non-local guesses:
	 -Rexp.BZERO.
	 -Rexp.BZERO#
	 -Rexp.BZERO().
	 -REG.BZERO.
	 -REG.BZERO#
	 -REG.BZERO().
	 -BZERO.
	 -BZERO#
	 -BZERO().
	 -scala/Predef.BZERO.
	 -scala/Predef.BZERO#
	 -scala/Predef.BZERO().

Document text:

```scala
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
      case BCHAR(_,ch) => 
        BCHAR(semiring.times(mark , if(ch == c) semiring.one else semiring.zero), c)

      case BALT(r1, r2) => BALT(shift(mark, r1, c), shift(mark, r2, c))

      case BSEQ(r1, r2) => BSEQ(shift(mark, r1, c),
        shift(    semiring.plus( semiring.times(mark, nullable(r1)), 
                                    fin(r1)), r2, c))
      case BSTAR(r) => BSTAR(shift(semiring.plus(mark, fin(r)), r, c))
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
```

#### Short summary: 

empty definition using pc, found symbol in pc: 