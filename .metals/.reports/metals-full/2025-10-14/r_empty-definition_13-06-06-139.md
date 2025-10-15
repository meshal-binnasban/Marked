error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts_Sets.sc:size.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts_Sets.sc
empty definition using pc, found symbol in pc: size.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -rexp/ms/size.
	 -rexp/ms/size#
	 -rexp/ms/size().
	 -ms/size.
	 -ms/size#
	 -ms/size().
	 -scala/Predef.ms.size.
	 -scala/Predef.ms.size#
	 -scala/Predef.ms.size().
offset: 216
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts_Sets.sc
text:
```scala
import scala.language.implicitConversions
import $file.rexp, rexp._

type Marks = Set[String]
type MarksPosition = Set[Int]

var S: String = "" 

// shifts function
def shifts(ms: Marks, r: Rexp): Marks = 
  if(ms.si@@ze >=100){
      println(s"ms=${ms.size}")
  }
  r match {
  case ZERO => Set.empty
  case ONE  => Set.empty
  case CHAR(c) =>for (m <- ms; if m != "" && m.head == c) yield m.tail

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
    
    if (ms1 == Set()) ms1 else
      ms1 ++ shifts(ms1, STAR(r))
  }
  case NTIMES(r, n) =>
    if (n == 0) ms
    else {
      val ms1 = shifts(ms, r)
      println(s"ms1= $ms1")
      if (ms1 == Set()) ms1
      else if (nullable(r)) ms1 ++ shifts(ms1, NTIMES(r, n - 1))
      else shifts(ms1, NTIMES(r, n - 1))
    }
}

// the main matching function 
def matcher(r: Rexp, s: String): Boolean =
  if (s == "") nullable(r)
  else shifts(Set(s), r).contains("")

def mat(r: Rexp, s: String): Marks =  
  shifts(Set(s), r)



@main
def test1() = {
  println("=====Test====")
  val r = %( %( "a" ) | %( "aa" ) | %( "aaa" ) | %( "aaaa" ) | %( "aaaaa" ) )
  val s = "a" * 100
  println("=string=")
  println(s)
  println(matcher(r,s))
}

@main
def test2() = {
  println("=====Test====")
  val reg = STAR(mkalts(100))
  println(s"reg=$reg") 
   for (n <- (0 to 100 by 100)) {
      println(s"$n ${ matcher(reg, "a" * n)}")
      println("-" * n)
   }
}

@main
def test3() = {
  println("=====Test====")
  val r = %(  "a" | "a" )
  val s = "a" * 3
  println("=string=")
  println(s)
  println(matcher(r,s))
}

@main
def test4() = {
  println("=====Test====")
  val r = %( "a" | "ac"   )
  val s = "ac" 
  println("=string=")
  println(s)
  println(matcher(r,s))
}
 
 def mkstar(n: Int) = STAR("a" * n)
def mkalts(n: Int) = {
  (for (i <- (1 to n).toList) yield mkstar(i)).reduceLeft(ALT.apply)
}




```


#### Short summary: 

empty definition using pc, found symbol in pc: size.