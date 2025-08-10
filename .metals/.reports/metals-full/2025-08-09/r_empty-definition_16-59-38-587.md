error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/Report/Versions/BitCoded_1.sc:`<none>`.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/Report/Versions/BitCoded_1.sc
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -rexp/enumerate.
	 -rexp/enumerate#
	 -rexp/enumerate().
	 -$file/enumerate.
	 -$file/enumerate#
	 -$file/enumerate().
	 -enumerate/enumerate.
	 -enumerate/enumerate#
	 -enumerate/enumerate().
	 -enumerate.
	 -enumerate#
	 -enumerate().
	 -scala/Predef.enumerate.
	 -scala/Predef.enumerate#
	 -scala/Predef.enumerate().
offset: 89
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/Report/Versions/BitCoded_1.sc
text:
```scala
import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.enumerat@@e, enumerate._
import $file.regenerate//, regenerate._
import $file.rebit


def shift(m: Boolean, bs: Bits, r: Rexp, c: Char) : Rexp = (r: @unchecked) match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if (m && d == c) POINT(bs, CHAR(d)) else CHAR(d)
  case POINT(_, CHAR(d)) => if (m && d == c) POINT(bs, CHAR(d)) else CHAR(d)
  case ALT(r1, r2) => ALT(shift(m, bs :+ Z, r1, c), shift(m, bs :+ S, r2, c))
  case SEQ(r1, r2) if m && nullable(r1) => SEQ(shift(m, bs, r1, c), shift(true, bs ::: mkeps(r1), r2, c))
  case SEQ(r1, r2) if fin(r1) => SEQ(shift(m, bs, r1, c), shift(true, mkfin(r1), r2, c))
  case SEQ(r1, r2) => SEQ(shift(m, bs, r1, c), shift(false, Nil, r2, c))
  case STAR(r) if m && fin(r) => STAR(shift(true, bs ::: (mkfin(r) :+ Z), r, c))
  case STAR(r) if fin(r) => STAR(shift(true, mkfin(r) :+ Z, r, c)) 
  case STAR(r) if m => STAR(shift(m, bs, r, c))
  case STAR(r) => STAR(shift(false, Nil, r, c))
}


```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.