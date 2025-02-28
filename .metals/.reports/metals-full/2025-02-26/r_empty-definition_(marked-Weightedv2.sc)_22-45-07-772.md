error id: scala/math/
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-Weightedv2.sc
empty definition using pc, found symbol in pc: scala/math/
empty definition using semanticdb
|empty definition using fallback
non-local guesses:
	 -

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
  REGW(semiring.zero, semiring.zero, ZEROw())
}
//Smart Constructors / how to calculate final and empty From the Article

//emptyw = one,finalw = zero, regw = EPSw
def onew[C, S](using semiring: Semiring[S]): REGW[C, S] = {
  REGW(semiring.one, semiring.zero, ONEw())
}

// emptyw = zero, finalw = zero, regw = SYMw f
def charw[C, S](f: C => S)(using semiring: Semiring[S]): REGW[C, S] = {
  REGW(semiring.zero, semiring.zero, CHARw(f))
}


def chari[S: SemiringI](c: Char): REGW[(Int, Char), S] = { 
    val sri = summon[SemiringI[S]]
    val sr = summon[Semiring[S]]   

    def weight(t: (Int, Char)): S = {
      val (pos, x) = t
      if (x == c) sri.index(pos)
      else sr.zero
    }
    //REGW(semiring.zero, semiring.zero, CHARw(weight))
    charw(weight)
}


// emptyw = emptyw p ⊕ emptyw q, finalw = finalw p ⊕finalw q, regw = ALTw p q
def altw[C, S](r1: REGW[C, S], r2: REGW[C, S])(using semiring: Semiring[S]): REGW[C, S] = {
  REGW(semiring.plus(r1.emptyw,r2.emptyw) 
        , semiring.plus(r1.finalw,r2.finalw)
          , ALTw(r1,r2))
}

// emptyw = emptyw p ⊗ emptyw q, finalw = finalw p ⊗ emptyw q ⊕finalw q, regw = SEQw p q
def seqw[C, S](r1: REGW[C, S], r2: REGW[C, S])(using semiring: Semiring[S]): REGW[C, S] = {
  REGW(semiring.times(r1.emptyw, r2.emptyw) 
        , semiring.plus(semiring.times(r1.finalw, r2.emptyw), r2.finalw)
          , SEQw(r1,r2))
}

// emptyw = one,finalw = finalw r, regw = REPw r
def starw[C, S](r1: REGW[C, S])(using semiring: Semiring[S]): REGW[C, S] = {
  REGW(semiring.one, r1.finalw , STARw(r1))
}

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
    def times(a: Int, b: Int): Int = a * b
  }

trait SemiringI[S] extends Semiring[S] {
    def index(i: Int): S
  }
//from article but i couldn't find a use to it.


/*
data Leftmost= NoLeft |Leftmost Start
data Start= NoStart |Start Int
*/

sealed trait StartT
case object NoStart extends StartT
case class Start(pos: Int) extends StartT

sealed trait LeftmostT
case object NoLeft extends LeftmostT
case class Leftmost(s: StartT) extends LeftmostT

/* 
instance Semiring Leftmost where
zero = NoLeft
one = Leftmost NoStart
NoLeft ⊕x = x
x ⊕NoLeft= x
Leftmost x ⊕Leftmost y= Leftmost (leftmost x y)
where leftmost NoStart NoStart= NoStart
leftmost NoStart (Start i) = Start i
leftmost (Start i) NoStart= Start i
leftmost (Start i) (Start j) = Start (min i j)

 */
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
        case (Start(i), NoStart) => Start(i)
        case (Start(i), Start(j)) => Start(min(i, j))
    }

   def index(i: Int): LeftmostT = Leftmost(Start(i)) 
}



def matcher[C, S](r: REGW[C, S], s: List[C])(using semiring: Semiring[S]): S = {
    s match {
      case Nil => r.emptyw
      case c :: cs => val  x =cs.foldLeft(shift(semiring.one, r.re, c))
        ((r, c) => shift(semiring.zero, r.re, c))
        x.finalw
    }
  }

def submatcher[C, S](r: REGW[(Int, C), S], s: List[C])(using semiring: Semiring[S]): S = {
    val arb: REGW[(Int, C), S] = starw(charw(_ => semiring.one))
    println((s.indices zip s).toList)
    matcher(seqw(arb, seqw(r, arb)), (s.indices zip s).toList)
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


def weighted[S](r: Rexp)(using semiring: Semiring[S]): REGW[Char, S] = r match {
  case ZERO      => zerow//[Char, S]
  case ONE       => onew//[Char, S]
  case CHAR(c)   => charw(x => if (x == c) semiring.one else semiring.zero)
  case ALT(r1, r2)  => altw(weighted(r1), weighted(r2))
  case SEQ(r1, r2)  => seqw(weighted(r1), weighted(r2))
  case STAR(r1)     => starw(weighted(r1))
}

@main
def test0() = {

val a = chari('a')(using semiringLeftmost)
//val ab = starw(altw(a, chari('b')(using semiringI)))
//val aaba = seqw(a, seqw (ab, a))(using semiringI)

   // println(submatcher(a, "bbabbb".toList)(using semiringInt))
}


@main
def test1() = {

val a = charw(_ == 'a')
val b= charw(_ == 'b')
val reg=seqw(a,b)

println("testing new implementation")
println(matcher(reg, "ab".toList))

val r = weighted ( STAR(ALT(CHAR('a'), CHAR('b')))) (using semiringInt)

println(matcher(r, "ababababababababa".toList))
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















  /* 
  data REGw c s = REGw {emptyw :: s,
finalw :: s,
regw :: REw c s}
data REw c s = EPSw
 SYMw (c →s)
 ALTw (REGw c s) (REGw c s)
 SEQw (REGw c s) (REGw c s)
 REPw (REGw c s)
epsw :: Semiring s ⇒REGw c s
epsw = REGw {emptyw = one,
finalw = zero,
regw = EPSw }
symw :: Semiring s ⇒(c →s)→REGw c s
symw f= REGw {emptyw = zero,
finalw = zero,
regw = SYMw f }
altw :: Semiring s ⇒REGw c s →REGw c s →REGw c s
altw p q= REGw {emptyw = emptyw p ⊕emptyw q,
finalw = finalw p ⊕finalw q,
regw = ALTw p q}
seqw :: Semiring s ⇒REGw c s →REGw c s →REGw c s
seqw p q=
REGw {emptyw = emptyw p ⊗emptyw q,
finalw = finalw p ⊗emptyw q ⊕finalw q,
regw = SEQw p q}
repw :: Semiring s ⇒REGw c s →REGw c s
repw r = REGw {emptyw = one,
finalw = finalw r,
regw = REPw r}
matchw :: Semiring s ⇒REGw c s →[c]→s
matchw r [ ] = emptyw r
matchw r (c : cs) =
finalw (foldl (shiftw zero·regw ) (shiftw one (regw r) c) cs)
shiftw :: Semiring s ⇒s →REw c s →c →REGw c s
shiftw EPSw = epsw
shiftw m (SYMw f) c = (symw f) {finalw = m ⊗f c}
shiftw m (ALTw p q) c =
altw (shiftw m (regw p) c) (shiftw m (regw q) c)
shiftw m (SEQw p q) c =
seqw (shiftw m (regw p) c)
(shiftw (m ⊗emptyw p ⊕finalw p) (regw q) c)
shiftw m (REPw r) c =
repw (shiftw (m ⊕finalw r) (regw r) c)
  
   */
```

#### Short summary: 

empty definition using pc, found symbol in pc: scala/math/