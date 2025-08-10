error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Play.sc:toList.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Play.sc
empty definition using pc, found symbol in pc: toList.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -rexp/s/toList.
	 -rexp/s/toList#
	 -rexp/s/toList().
	 -s/toList.
	 -s/toList#
	 -s/toList().
	 -scala/Predef.s.toList.
	 -scala/Predef.s.toList#
	 -scala/Predef.s.toList().
offset: 1020
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Play.sc
text:
```scala
import $file.rexp, rexp._

def fin(r: Rexp) : Boolean = (r: @unchecked) match {
  case ZERO => false
  case ONE => false
  case CHAR(_) => false
  case POINT(CHAR(_)) => true
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
}

def shift(m: Boolean, c: Char, r: Rexp ) : Rexp = (r: @unchecked) match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if (m && d == c) POINT(CHAR(d)) else CHAR(d)
  case POINT(CHAR(d)) => if (m && d == c) POINT(CHAR(d)) else CHAR(d)
  case ALT(r1, r2) => ALT(shift(m, c, r1), shift(m, c, r2))
  case SEQ(r1, r2) => SEQ(shift(m, c, r1), shift((m && nullable(r1)) || fin(r1), c, r2))
  case STAR(r) => STAR(shift(m || fin(r), c, r))
}

def mat(r: Rexp, s: List[Char]) : Rexp = s match {
  case Nil => r
  case c::cs => cs.foldLeft(shift(true, c, r))((r, c) => shift(false,c,r))
}

def matcher(r: Rexp, s: String) : Boolean =
  if (s.toList == Nil) nullable(r) else fin(mat(r, s.t@@oList))

@main
def test1() = {
  println("=====Test====")
  val r = ("a" | "ab") ~ ("c" | "bc")
  val s = "abc"
  println("=string=")
  println(s)
  println(matcher(r,s))
}




```


#### Short summary: 

empty definition using pc, found symbol in pc: toList.