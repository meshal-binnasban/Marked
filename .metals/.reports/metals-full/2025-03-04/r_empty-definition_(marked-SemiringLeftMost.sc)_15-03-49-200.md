error id: plus.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-SemiringLeftMost.sc
empty definition using pc, found symbol in pc: plus.
empty definition using semanticdb
|empty definition using fallback
non-local guesses:
	 -Rexp.semi.plus.
	 -Rexp.semi.plus#
	 -Rexp.semi.plus().
	 -REG.semi.plus.
	 -REG.semi.plus#
	 -REG.semi.plus().
	 -semi/plus.
	 -semi/plus#
	 -semi/plus().
	 -scala/Predef.semi.plus.
	 -scala/Predef.semi.plus#
	 -scala/Predef.semi.plus().

Document text:

```scala
import scala.compiletime.ops.boolean
import scala.math.min
import scala.language.implicitConversions

enum Rexp[C, S] {
  case ZERO()
  case ONE() 
  case CHAR(f: C => S)
  case ALT(r1: Rexp[C, S], r2: Rexp[C, S])
  case SEQ(r1: Rexp[C, S], r2: Rexp[C, S])
  case STAR(r: Rexp[C, S])
}

import Rexp._
def OPT[C,S](r: Rexp[C,S]) = ALT(r, ONE)


enum REG[C,S] {
  case BZERO()
  case BONE() 
  case BCHAR(b: S, f: C => S)
  case BALT(r1: REG[C,S], r2: REG[C,S])
  case BSEQ(r1: REG[C,S], r2: REG[C,S])
  case BSTAR(r: REG[C,S])
  case BINIT(r: REG[C,S])
}
import REG._

def nullable[C,S](r: REG[C,S])(using semiring: SemiringI[S]) : S = r match {
  case BZERO() => semiring.zero
  case BONE()=> semiring.one
  case BCHAR(_,f) =>  semiring.zero
  case BALT(r1, r2) => semiring.plus(nullable(r1),nullable(r2))
  case BSEQ(r1, r2) => semiring.times(nullable(r1),nullable(r2))
  case BSTAR(r) => semiring.one
  case BINIT(r) => nullable(r)
}

def fin[C,S](r: REG[C,S])(using semiring: SemiringI[S]) : S = r match {
  case BZERO() => semiring.zero
  case BONE() => semiring.zero
  case BCHAR(b,_) => b
  case BALT(r1, r2) => semiring.plus( fin(r1) , fin(r2) )
  case BSEQ(r1, r2) => semiring.plus(semiring.times(fin(r1),nullable(r2)),fin(r2))
  case BSTAR(r) => fin(r)
}

def shift[C,S](mark: S, re: REG[C,S], c: C)(using semiring: SemiringI[S]): REG[C,S] = {
    re match {
      case BZERO() => BZERO() //re
      case BONE() => BONE() //re
      case BCHAR(b,ch) => 
        c match {
            case (pos: Int, x: Char) => 
                if (x == ch)
                    {val m=semiring.times(mark,semiring.index(pos))
                     BCHAR(m , c) 
                }
                else BCHAR(semiring.times(mark,semiring.zero) , c)
            case _ =>
                println(mark)
                println(c==ch)
                //println(semiring.one) 
                BCHAR(semiring.times(mark , if(c == ch) semiring.one else semiring.zero), c) //incase of booleansemiring
        }
      case BALT(r1, r2) => BALT(shift(mark, r1, c), shift(mark, r2, c))

      case BSEQ(r1, r2) => BSEQ(shift(mark, r1, c),
        shift(    semiring.plus( semiring.times(mark, nullable(r1)), 
                                    fin(r1)), r2, c))
      case BSTAR(r) => BSTAR(shift(semiring.plus(mark, fin(r)), r, c))
      case BINIT(r) => shift(semiring.one, r, c)
    }
  }

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

def mat[C,S](r: REG[C,S], s: List[C])(using semiring: SemiringI[S]) : REG[C,S] = s match {
  case Nil => r
  case c::cs => mat(shift(semiring.zero, r, c), cs)
}

def matcher[C,S](r: REG[C,S], s: List[C])(using semiring: SemiringI[S]) : S =
  if (s == Nil) nullable(r) 
    else fin(mat(r, s))

/*
def matcher[C, S](r: REG[C, S], s: List[C])(using semiring: SemiringI[S]): S = {
    s match {
      case Nil => fin(r)
      case c :: cs => val  x =cs.foldLeft(shift(semiring.one, r.re, c))
        ((r, c) => shift(semiring.zero, r.re, c))
        x.finalw
    }
}
*/

/*
def submatcher[C, S](r: REGW[C, S], s: List[C])(using semiring: SemiringI[S]): S = {
    val arb: REGW[C, S] = starw(charw(_ => semiring.one))
    matcher(seqw(arb, seqw(r, arb)), (s.indices zip s).toList.asInstanceOf[List[C]])  
}
*/

// internalises a regular expression into a marked
// regular expression
def intern[C,S](r: Rexp)(using semiring: SemiringI[S]) : REG[C,S] = r match {
  case ZERO => BZERO()
  case ONE => BONE()
  case CHAR(f) => BCHAR(semiring.zero, f)
  case ALT(r1, r2) => BALT(intern(r1), intern(r2))
  case SEQ(r1, r2) => BSEQ(intern(r1), intern(r2))
  case STAR(r) => BSTAR(intern(r))
}



// make sure the outermost REG is marked
def intern2[C,S](r: Rexp)(using semiring: SemiringI[S]) : REG[Char,S] = BINIT(intern(r))

@main
def test1() = {

    val br1 = SEQ("a", SEQ("b", "c"))
    val br2 = intern2(br1)(using semiringILeftmost)
    println(br2)
    val s = "abc".toList
    println(matcher(br2,s))

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
 /*
  for (i <- 0 to 7000000 by 500000) {
    println(f"$i: ${time_needed(2, matcher(weighted(EVIL2), ("a" * i).toList))}%.5f")
  }
    */
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

empty definition using pc, found symbol in pc: plus.