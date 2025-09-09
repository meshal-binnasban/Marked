error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts.sc:prune2.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts.sc
empty definition using pc, found symbol in pc: prune2.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -rexp/ms1/prune2.
	 -rexp/ms1/prune2#
	 -rexp/ms1/prune2().
	 -ms1/prune2.
	 -ms1/prune2#
	 -ms1/prune2().
	 -scala/Predef.ms1.prune2.
	 -scala/Predef.ms1.prune2#
	 -scala/Predef.ms1.prune2().
offset: 770
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts.sc
text:
```scala
import scala.language.implicitConversions
import $file.rexp, rexp._

type Marks = List[String]

// shifts function 
def shifts(ms: Marks, r: Rexp) : Marks = 
  if (ms == Nil) Nil else {
    r match {
      case ZERO => Nil
      case ONE => Nil
      case CHAR(c) => for (m <- ms; if m != "" && m.head == c) yield m.tail
      case ALT(r1, r2) => shifts(ms, r1) ::: shifts(ms, r2)
      case SEQ(r1, r2) => {
        val ms1 = shifts(ms, r1).prune2
        (nullable(r1), nullable(r2)) match {
          case (true, true) =>  (shifts((ms1.prune2 ::: ms.prune2).prune2, r2) ::: ms1.prune2).prune2
          case (true, false) => shifts((ms1.prune2 ::: ms.prune2).prune2, r2).prune2 
          case (false, true) => (shifts(ms1.prune2, r2) ::: ms1.prune2@@).prune2
          case (false, false) => shifts(ms1, r2).p
        }
      }
      case STAR(r) => {
        val ms1 = shifts(ms, r)
        ms1 ::: shifts(ms1, STAR(r)) 
      }
    }
  }

// the main matching function 
def matcher(r: Rexp, s: String) : Boolean = {
  if (s == "") nullable(r)
  else 
    shifts(List(s), r).exists(_ == "")
}

extension (ms: Marks)
  def prune2: Marks =
    var seen = Set.empty[String]
    ms.filter { s =>
      val keep = !seen.contains(s)
      if (keep) seen += s
      keep
}

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

empty definition using pc, found symbol in pc: prune2.