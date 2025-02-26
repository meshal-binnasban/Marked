error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-Weightedv1.sc:4
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-Weightedv1.sc
empty definition using pc, found symbol in pc: 
semanticdb not found
|empty definition using fallback
non-local guesses:
	 -S#
	 -scala/Predef.S#

Document text:

```scala
enum REG[C, S](val emptyw: S, val finalw: S,val regw: RE[C, S])


enum REw[C, S] {
  case EPSw() extends REw[C, S]
  case SYMw(f: C => S) extends REw[C, S]
  case ALTw(a: REGw[C, S], b: REGw[C, S]) extends REw[C, S]
  case SEQw(a: REGw[C, S], b: REGw[C, S]) extends REw[C, S]
  case REPw(a: REGw[C, S]) extends REw[C, S]
}

```

#### Short summary: 

empty definition using pc, found symbol in pc: 