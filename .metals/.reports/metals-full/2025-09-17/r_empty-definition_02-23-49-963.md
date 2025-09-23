error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts_Sets.sc:`<none>`.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts_Sets.sc
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb

found definition using fallback; symbol String
offset: 1041
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts_Sets.sc
text:
```scala
import scala.language.implicitConversions
import $file.rexp, rexp._

type Marks = Set[String]

// shifts function
def shifts(ms: Marks, r: Rexp): Marks = r match {
  case ZERO => Set()
  case ONE  => Set()
  case CHAR(c) => for (m <- ms; if m != "" && m.head == c) yield m.tail
  case ALT(r1, r2) => shifts(ms, r1) ++ shifts(ms, r2)
  case SEQ(r1, r2) => {
    val ms1 = shifts(ms, r1)
    (nullable(r1), nullable(r2)) match {
      case (true,  true)  => shifts(ms1 ++ ms, r2) ++ ms1
      case (true,  false) => shifts(ms1 ++ ms, r2)
      case (false, true)  => shifts(ms1, r2) ++ ms1
      case (false, false) => shifts(ms1, r2)
    }
  }
  case STAR(r) => {
    val ms1 = shifts(ms, r)
    if (ms1 == Set()) ms1 else ms1 ++ shifts(ms1, STAR(r))
  }
  case NTIMES(r, n) =>
    if (n == 0) ms
    else {
      val ms1 = shifts(ms, r)
      if (ms1 == Set()) ms1
      else if (nullable(r)) ms1 ++ shifts(ms1, NTIMES(r, n - 1))
      else shifts(ms1, NTIMES(r, n - 1))
    }
}

// the main matching function 
def matcher(r: Rexp, s: String@@): Boolean =
  if (s == "") nullable(r)
  else shifts(Set(s), r).contains("")

@main
def test1() = {
  println("=====Test====")
  val r = (("a") | (ONE)) ~ (("a") | NTIMES("a",3))
    //aaa
    
  val s = "aaa"
  println("=string=")
  println(s)
  println(matcher(r,s))
}

@main
def test3() = {
  println("=====Test====")
  val r = %( %( "a" ) | %( "aa" ) | %( "aaa" ) | %( "aaaa" ) | %( "aaaaa" ) )
  val s = "a" * 1000
  println("=string=")
  println(s)
  println(matcher(r,s))
}
 



```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.