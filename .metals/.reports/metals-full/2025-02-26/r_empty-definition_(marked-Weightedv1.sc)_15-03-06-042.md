error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-Weightedv1.sc:39
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-Weightedv1.sc
empty definition using pc, found symbol in pc: 
semanticdb not found
|empty definition using fallback
non-local guesses:
	 -ALT.
	 -ALT#
	 -ALT().
	 -scala/Predef.ALT.
	 -scala/Predef.ALT#
	 -scala/Predef.ALT().

Document text:

```scala


case class REGW[C, S](emptyw: S, finalw: S, re: RE[C, S])

sealed trait RE[C, S]
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
          , ALT(r1,r2))
}

// emptyw = one,finalw = finalw r, regw = SEQw p q
def star[C, S](r1: REGW[C, S])(using semiring: Semiring[S]): REGW[C, S] = {
  REGW(semiring.one,, ALT(r1,r2))
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
```

#### Short summary: 

empty definition using pc, found symbol in pc: 