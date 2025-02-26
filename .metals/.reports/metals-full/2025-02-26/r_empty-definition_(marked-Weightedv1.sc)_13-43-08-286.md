error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-Weightedv1.sc:9
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-Weightedv1.sc
empty definition using pc, found symbol in pc: 
semanticdb not found
|empty definition using fallback
non-local guesses:
	 -C#
	 -scala/Predef.C#

Document text:

```scala



enum REG[C, S](emptyw: S,finalw: S,regw: RE[C, S])


enum RE[C, S] {
  case ONE() extends REw[C, S]
  case SYMw(f: C => S) extends REw[C, S]
  case ALTw(a: REGw[C, S], b: REGw[C, S]) extends REw[C, S]
  case SEQw(a: REGw[C, S], b: REGw[C, S]) extends REw[C, S]
  case REPw(a: REGw[C, S]) extends REw[C, S]
}

```

#### Short summary: 

empty definition using pc, found symbol in pc: 