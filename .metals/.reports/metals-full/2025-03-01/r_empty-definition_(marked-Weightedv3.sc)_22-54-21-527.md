error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-Weightedv3.sc:253
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-Weightedv3.sc
empty definition using pc, found symbol in pc: 
semanticdb not found
|empty definition using fallback
non-local guesses:
	 -Rexp.semiringILeftlong.
	 -Rexp.semiringILeftlong#
	 -Rexp.semiringILeftlong().
	 -semiringILeftlong.
	 -semiringILeftlong#
	 -semiringILeftlong().
	 -scala/Predef.semiringILeftlong.
	 -scala/Predef.semiringILeftlong#
	 -scala/Predef.semiringILeftlong().

Document text:

```scala
import scala.compiletime.ops.boolean
import scala.math.min

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

case class REGW[C, S](emptyw: S, finalw: S, re: REw[C, S])
sealed trait REw[C, S]
case class ZEROw[C, S]() extends REw[C, S]
case class ONEw[C, S]() extends REw[C, S]
case class CHARw[C, S](f: C => S) extends REw[C, S]
case class ALTw[C, S](a: REGW[C, S], b: REGW[C, S]) extends REw[C, S]
case class SEQw[C, S](a: REGW[C, S], b: REGW[C, S]) extends REw[C, S]
case class STARw[C, S](a: REGW[C, S]) extends REw[C, S]

def zerow[C, S](using semiring: Semiring[S]): REGW[C, S] = {
  REGW(semiring.zero, semiring.zero, ZEROw()) }
//Smart Constructors page 7 -  how to calculate final and empty From the Article
def onew[C, S](using semiring: Semiring[S]): REGW[C, S] = {
  REGW(semiring.one, semiring.zero, ONEw()) }

def charw[C, S](f: C => S)(using semiring: Semiring[S]): REGW[C, S] = {
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
def altw[C, S](r1: REGW[C, S], r2: REGW[C, S])(using semiring: Semiring[S]): REGW[C, S] = {
  REGW(semiring.plus(r1.emptyw,r2.emptyw) 
        , semiring.plus(r1.finalw,r2.finalw)
          , ALTw(r1,r2))
}
def seqw[C, S](r1: REGW[C, S], r2: REGW[C, S])(using semiring: Semiring[S]): REGW[C, S] = {
  REGW(semiring.times(r1.emptyw, r2.emptyw) 
        , semiring.plus(semiring.times(r1.finalw, r2.emptyw), r2.finalw)
          , SEQw(r1,r2))
}
def starw[C, S](r1: REGW[C, S])(using semiring: Semiring[S]): REGW[C, S] = {
  REGW(semiring.one, r1.finalw , STARw(r1))  }

trait Semiring[S] {
  def zero: S   // Additive identity
  def one: S    // Multiplicative identity
  def plus(a: S, b: S): S  // ⊕ Addition
  def times(a: S, b: S): S // ⊗ Multiplication
}
given booleanSemiring: Semiring[Boolean] with {
  def zero: Boolean = false
  def one: Boolean = true
  def plus(a: Boolean, b: Boolean): Boolean = a || b
  def times(a: Boolean, b: Boolean): Boolean = a && b
}

// from article but i couldn't find a use to it.
given semiringInt: Semiring[Int] with {
    def zero = 0
    def one = 1
    def plus(a: Int, b: Int): Int = a + b
    def times(a: Int, b: Int): Int =  a * b
  }
trait SemiringI[S] extends Semiring[S] {
    
    def index(i: Int): S
  }

given semiringIntI: SemiringI[Int] with {
    export semiringInt.*// Inherit all `Semiring` operations
    def index(i: Int): Int = i 
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

/*
data LeftLong= NoLeftLong |LeftLong Range
data Range= NoRange |Range Int Int
*/
sealed trait RangeT
case object NoRange extends RangeT
case class Range(start: Int, end:Int) extends RangeT
sealed trait LeftlongT
case object NoLeftLong extends LeftlongT
case class LeftLong(range: RangeT) extends LeftlongT

given semiringLeftlong: Semiring[LeftlongT] with {
  def zero =  NoLeftLong 
  def one = LeftLong(NoRange) 

  def plus(a: LeftlongT, b: LeftlongT): LeftlongT = (a,b) match{
    case (NoLeftLong, x) => x
    case (x, NoLeftLong) => x
    case (LeftLong(x), LeftLong(y)) => LeftLong(leftlong(x, y))
  } 
  def times(a: LeftlongT, b: LeftlongT): LeftlongT = (a,b) match {
        case (NoLeftLong, _) => NoLeftLong
        case (_, NoLeftLong) => NoLeftLong
        case (LeftLong(x), LeftLong(y)) =>  LeftLong(range(x, y))
  }
       
/* p.8: we pick the longer leftmost match rather than only considering the start position
i <k ∨ i=k ∧j l= Range i j
otherwise= Range k l */
  def leftlong(x: RangeT,y:RangeT): RangeT = (x,y) match {
        case (NoRange, NoRange) => NoRange
        case (NoRange, Range(i,j)) => Range(i,j)
        case (Range(i,j),NoRange) => Range(i,j)
        case (Range(i,j), Range(k,l)) => {
            if(i<k || (i == k && j >= l)) Range(i,j) else Range(k,l) 
        }
    }
   /* 
p.8: we pick the start position of the first part and the end position of the second part
LeftLong x ⊗LeftLong y= LeftLong (range x y)
where range... range (Range i ) (Range j) = Range i j */ 
  def range(x: RangeT,y:RangeT): RangeT = {(x,y) match {
        case (NoRange, NoRange) => NoRange
        case (NoRange, range) => range
        case (range, NoRange) => range
        case (Range(i,_), Range(_,j)) => 
            Range(i,j)
        }
    }
}
//instance Semiringi LeftLong where index i= LeftLong (Range i i)
given semiringILeftlong: SemiringI[LeftlongT] with {
    export semiringLeftlong.*// Inherit all `Semiring` operations
    def index(i:Int): LeftlongT = LeftLong(Range(i,i))
}

def matcher[C, S](r: REGW[C, S], s: List[C])(using semiring: Semiring[S]): S = {
    s match {
      case Nil => r.emptyw
      case c :: cs => val  x =cs.foldLeft(shift(semiring.one, r.re, c))
        ((r, c) => shift(semiring.zero, r.re, c))
        x.finalw
    }
}

// REG C ,S also inside 
def submatcher[C, S](r: REGW[C, S], s: List[C])(using semiring: Semiring[S]): S = {
    val arb: REGW[C, S] = starw(charw(_ => semiring.one))
    semiring match {
        case si: SemiringI[S] => 
            matcher(seqw(arb, seqw(r, arb)), (s.indices zip s).toList.asInstanceOf[List[C]])
        case _: Semiring[S] =>
             matcher(seqw(arb, seqw(r, arb)), s)
    }
}

def shift[C, S](mark: S, re: REw[C, S], c: C)(using semiring: Semiring[S]): REGW[C, S] = {
    re match {
      case ZEROw() => zerow[C, S]
      case ONEw() => onew[C, S]
      case CHARw(f) => REGW(semiring.zero, semiring.times(mark, f(c)), CHARw(f)) 
      case ALTw(r1, r2) => altw(shift(mark, r1.re, c), shift(mark, r2.re, c))
      case SEQw(r1, r2) => seqw(shift(mark, r1.re, c),
        shift(semiring.plus( semiring.times(mark, r1.emptyw), 
                                    r1.finalw), r2.re, c))
      case STARw(r) => starw(shift(semiring.plus(mark, r.finalw), r.re, c))
    }
  }

def weighted[S](r: Rexp)(using semiring: Semiring[S]): REGW[Char,S] = r match {
  case ZERO      => zerow//[C, S]
  case ONE       => onew//[C, S]
  case CHAR(c)   => 
    //charw(x => if (x == c) semiring.one else semiring.zero)
    semiring match {
        case si: SemiringI[S] => chari(c)(using si)
        case _: Semiring[S] => charw(x => if (x == c) semiring.one else semiring.zero)
    }
  case ALT(r1, r2)  => altw(weighted(r1), weighted(r2))
  case SEQ(r1, r2)  => seqw(weighted(r1), weighted(r2))
  case STAR(r1)     => starw(weighted(r1))
}

@main
def test0() = {

val a = weighted(CHAR('a')) (using booleanSemiring)
val aStar = starw(a)

val b = weighted(CHAR('b')) (using booleanSemiring)
val bStar=starw(b)

val r= seqw(aStar,b)
val s ="hello athisbb isabb test of left most with ab"
println(matcher(r, s.toList)(using booleanSemiring))

//val r2 = weighted ( STAR(ALT(CHAR('a'), CHAR('b'))))(using semiringILeftmost)
//println(submatcher(r2, "hello ab test abab".toList)(using semiringILeftmost))

}

@main
def test1() = {
    //booleanSemiring
    //semiringILeftmost
    //semiringILeftlong
   
    val aStarB = CHAR('a')% ~ CHAR('b') 
    println(string(aStarB))
    val reg=weighted(aStarB) (using  semiringILeftlong)
    val s="aab aaab b"
    println(submatcher(reg,s.toList))

    

  /* article test
   val a= CHAR('a')
   val ab=STAR(ALT(a, CHAR('b')))
   val aaba= SEQ(a, SEQ( ab,a ) )
   val reg=weighted(aaba) (using  semiringILeftlong)
   val s="bababa"
   submatcher(reg,s.toList)
*/
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
    println(f"$i: ${time_needed(2, matcher(weighted(EVIL2)(using booleanSemiring), ("a" * i).toList))}%.5f")
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