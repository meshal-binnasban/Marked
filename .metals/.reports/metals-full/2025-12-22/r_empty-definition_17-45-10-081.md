error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/rexps.sc:`<none>`.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/rexps.sc
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -rexp/$file.
	 -$file.
	 -scala/Predef.$file.
offset: 7
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/rexps.sc
text:
```scala
import @@$file.rexp, rexp._
// ================= Rexp with ids =================
abstract class RexpS
case object ZEROS extends RexpS
case object ONES  extends RexpS
case class CHARS(c: Char) extends RexpS
case class ALTS(r1: RexpS, r2: RexpS) extends RexpS
case class SEQS(r1: RexpS, r2: RexpS, id: Int) extends RexpS
case class STARSS(r: RexpS, id: Int) extends RexpS
case class NTIMESS(r: RexpS, n: Int) extends RexpS
case class ANDS(r1: RexpS, r2: RexpS) extends RexpS

def intern(r: Rexp): RexpS = internalize(r, 0)._1

def internalize(r: Rexp, id: Int): (RexpS, Int) = r match {
  case ZERO => (ZEROS, id)
  case ONE => (ONES, id)
  case CHAR(c) => (CHARS(c), id)
  case ALT(r1, r2) =>
    val (r1s, id1) = internalize(r1, id)
    val (r2s, id2) = internalize(r2, id1)
    (ALTS(r1s, r2s), id2)
  case SEQ(r1, r2) =>
    val (r1s, id1) = internalize(r1, id)
    val (r2s, id2) = internalize(r2, id1)
    (SEQS(r1s, r2s, id2), id2 + 1)
  case STAR(r) =>
    val (rs, id1) = internalize(r, id)
    (STARSS(rs, id1), id1 + 1)
  case NTIMES(r, n) =>
    val (rs, id1) = internalize(r, id)
    (NTIMESS(rs, n), id1)
  case AND(r1, r2) =>
    val (r1s, id1) = internalize(r1, id)
    val (r2s, id2) = internalize(r2, id1)
    (ANDS(r1s, r2s), id2)
}

def nullableS(r: RexpS): Boolean = r match {
  case ZEROS           => false
  case ONES            => true
  case CHARS(_)        => false
  case ALTS(r1, r2)    => nullableS(r1) || nullableS(r2)
  case SEQS(r1, r2, _) => nullableS(r1) && nullableS(r2)
  case STARSS(_, _)    => true
  case NTIMESS(r, n)   => n == 0 || nullableS(r)
  case ANDS(r1, r2)    => nullableS(r1) && nullableS(r2)
}

```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.