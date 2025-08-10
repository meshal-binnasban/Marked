error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Derivative.sc:`<none>`.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Derivative.sc
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -$file/rexp.
	 -$file/rexp#
	 -$file/rexp().
	 -rexp/rexp.
	 -rexp/rexp#
	 -rexp/rexp().
	 -rexp.
	 -rexp#
	 -rexp().
	 -scala/Predef.rexp.
	 -scala/Predef.rexp#
	 -scala/Predef.rexp().
offset: 13
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Derivative.sc
text:
```scala
import $file.@@rexp, rexp._

def der(c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) =>
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r) => SEQ(der(c, r), STAR(r))
  case NTIMES(r, n) => if (n == 0) ZERO else SEQ(der(c, r), NTIMES(r, n-1)) // new to testX1.
}

// the derivative w.r.t. a string (iterates der and simp)
def ders(s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, der(c, r))
}
```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.