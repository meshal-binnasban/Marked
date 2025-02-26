error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-Weightedv1.sc:26
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-Weightedv1.sc
empty definition using pc, found symbol in pc: 
semanticdb not found
|empty definition using fallback
non-local guesses:
	 -Rexp.REGW.
	 -Rexp.REGW#
	 -Rexp.REGW().
	 -REGW.
	 -REGW#
	 -REGW().
	 -scala/Predef.REGW.
	 -scala/Predef.REGW#
	 -scala/Predef.REGW().

Document text:

```scala

enum Rexp {
  case ZERO
  case ONE 
  case CHAR(c: Char)
  case ALT(r1: Rexp, r2: Rexp )
  case SEQ(r1: Rexp, r2: Rexp)
  case STAR(r: Rexp)
  //case NTIMES(r: Rexp, n: Int) 
}
import Rexp._



case class REGW[C, S](emptyw: S, finalw: S, re: RE[C, S])

sealed trait RE[C, S]
case class ZERO[C, S]() extends RE[C, S]
case class ONE[C, S]() extends RE[C, S]
case class CHAR[C, S](f: C => S) extends RE[C, S]
case class ALT[C, S](a: REGW[C, S], b: REGW[C, S]) extends RE[C, S]
case class SEQ[C, S](a: REGW[C, S], b: REGW[C, S]) extends RE[C, S]
case class STAR[C, S](a: REGW[C, S]) extends RE[C, S]


//Smart Constructors / how to calculate final and empty From the Article
//emptyw = one,finalw = zero, regw = EPSw
def one[C, S](using semiring: Semiring[S]): REGW[C, S] = {
  REGW(semiring.one, semiring.zero, ONE())
}

// emptyw = zero, finalw = zero, regw = SYMw f
def char[C, S](f: C => S)(using semiring: Semiring[S]): REGW[C, S] = {
  REGW(semiring.zero, semiring.zero, CHAR(f))
}

// emptyw = emptyw p ⊕ emptyw q, finalw = finalw p ⊕finalw q, regw = ALTw p q
def alt[C, S](r1: REGW[C, S], r2: REGW[C, S])(using semiring: Semiring[S]): REGW[C, S] = {
  REGW(semiring.plus(r1.emptyw,r2.emptyw) 
        , semiring.plus(r1.finalw,r2.finalw)
          , ALT(r1,r2))
}

// emptyw = emptyw p ⊗ emptyw q, finalw = finalw p ⊗ emptyw q ⊕finalw q, regw = SEQw p q
def seq[C, S](r1: REGW[C, S], r2: REGW[C, S])(using semiring: Semiring[S]): REGW[C, S] = {
  REGW(semiring.times(r1.emptyw, r2.emptyw) 
        , semiring.plus(semiring.times(r1.finalw, r2.emptyw), r2.finalw)
          , SEQ(r1,r2))
}

// emptyw = one,finalw = finalw r, regw = REPw r
def star[C, S](r1: REGW[C, S])(using semiring: Semiring[S]): REGW[C, S] = {
  REGW(semiring.one, r1.finalw , STAR(r1))
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

given semiringInt: Semiring[Int] with {
    def zero = 0
    def one = 1
    def plus(a: Int, b: Int): Int = a + b
    def times(a: Int, b: Int): Int = a * b
  }

trait SemiringI[S] extends Semiring[S] {
    def index(i: Int): S
  }

def matcher[C, S](r: REGW[C, S], s: List[C])(using semiring: Semiring[S]): S = {
    s match {
      case Nil => r.emptyw
      case c :: cs => val  x =cs.foldLeft(shift(semiring.one, r.re, c))
        ((r, c) => shift(semiring.zero, r.re, c))
        x.finalw
    }
  }

  def shift[C, S](mark: S, re: RE[C, S], c: C)(using semiring: Semiring[S]): REGW[C, S] = {
    re match {
      case ONE() => one
      case CHAR(f) => REGW(semiring.zero, semiring.times(mark, f(c)), CHAR(f)) 
      case ALT(r1, r2) => alt(shift(mark, r1.re, c), shift(mark, r2.re, c))
      case SEQ(r1, r2) => seq(shift(mark, r1.re, c),
        shift(semiring.plus( semiring.times(mark, r1.emptyw), 
                                    r1.finalw), r2.re, c))
      case STAR(r) => star(shift(semiring.plus(mark, r.finalw), r.re, c))
    }
  }

@main
def test1() = {
//val r = SEQ(SEQ(CHAR('a'), CHAR('b')) , STAR(CHAR('c')))
//matcher(r, "abc".toList)
//matcher(r, "abcccccccccccccccccc".toList)

val a = char(_ == 'a')
val b= char(_ == 'b')
val reg=seq(a,b)

println("testing new implementation")
matcher(reg, "abc".toList)

//val r= NTIMES(CHAR('b'),2)
//matcher(r, "bbc".toList)

}

def weighted[S](r: Rexp)(using semiring: Semiring[S]): REGW[Char, S] = r match {
  case ZERO      => ZEROw
  case ONE       => ONEw
  case CHAR(c)   => CHARw(x => if (x == c) semiring.one else semiring.zero)
  case ALT(r1, r2)  => ALTw(weighted(r1), weighted(r2))
  case SEQ(r1, r2)  => SEQw(weighted(r1), weighted(r2))
  case STAR(r1)     => STARw(weighted(r1))
}




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

empty definition using pc, found symbol in pc: 