error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts.sc:`<none>`.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts.sc
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -rexp/r.
	 -rexp/r#
	 -rexp/r().
	 -r.
	 -r#
	 -r().
	 -scala/Predef.r.
	 -scala/Predef.r#
	 -scala/Predef.r().
offset: 1960
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts.sc
text:
```scala
import $file.rexp, rexp._

import scala.language.implicitConversions

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
        val ms1 = shifts(ms, r1)
        (nullable(r1), nullable(r2)) match {
          case (true, true) =>  shifts(ms1 ::: ms, r2) ::: ms1
          case (true, false) => shifts(ms1 ::: ms, r2) 
          case (false, true) => shifts(ms1, r2) ::: ms1
          case (false, false) => shifts(ms1, r2)
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
    println(s"List Marks = ${shifts(List(s), r)}") 
    shifts(List(s), r).exists(_ == "")
}



@main
def test3() = {
  println("=====Test====")
  val r = SEQ(CHAR('a'),ALT(ONE,ONE))
  val s = "a"
  println("=string=")
  println(r)
  println(s)
  println(s"res: ${matcher(r, s)}")
}

@main
def test4() = {
  println("=====Test====")
  val r = (%(%(ONE))) ~ "c"
  val s = "c"
  println(r)
  println(s)
  println(s"res: ${matcher(r, s)}")
}

@main
def test5() = {
  println("=====Test====")
  val r = STAR("e" | ("d" | "c"))
  val s = "cdc"
  println("=string=")
  println(r)
  println(s)
  println(s"res: ${matcher(r, s)}")
}

@main
def test6() = {
  println("=====Test====")
  val r = STAR(ONE ~ ONE) ~ "cc"
  val s = "cc"
  println("=string=")
  println(r)
  println(s)
  println(s"res: ${matcher(r, s)}")
}

@main
def test7() = {
  println("=====Test====")
  val r = (%(%(ONE ~ ONE))) ~ "c"
  val s = "c"
  println(@@r)
  println(s)
  println(s"res: ${matcher(r, s)}")
}



```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.