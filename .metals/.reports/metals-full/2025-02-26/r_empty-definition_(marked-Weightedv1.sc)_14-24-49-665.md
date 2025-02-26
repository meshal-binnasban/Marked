error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-Weightedv1.sc:3
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-Weightedv1.sc
empty definition using pc, found symbol in pc: 
semanticdb not found
|empty definition using fallback
non-local guesses:
	 -S#
	 -scala/Predef.S#

Document text:

```scala


enum REG[C, S](emptyw: S,finalw: S, regw: RE[C, S]) {
  case REGCase(emptyw: S, finalw: S, regw: RE[C, S]) extends REG(emptyw, finalw, regw)
}

enum RE[C, S] {
  case ONE extends RE[C, S]
  case CHAR(f: C => S) extends RE[C, S]
  case ALT(r1: REG[C, S], r2: REG[C, S]) extends RE[C, S]
  case SEQ(r1: REG[C, S], r2: REG[C, S]) extends RE[C, S]
  case STAR(r1: REG[C, S]) extends RE[C, S]
}
```

#### Short summary: 

empty definition using pc, found symbol in pc: 