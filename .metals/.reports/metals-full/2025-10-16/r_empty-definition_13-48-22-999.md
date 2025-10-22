error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts.sc:isEmpty.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts.sc
empty definition using pc, found symbol in pc: isEmpty.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -rexp/ms1/isEmpty.
	 -rexp/ms1/isEmpty#
	 -rexp/ms1/isEmpty().
	 -ms1/isEmpty.
	 -ms1/isEmpty#
	 -ms1/isEmpty().
	 -scala/Predef.ms1.isEmpty.
	 -scala/Predef.ms1.isEmpty#
	 -scala/Predef.ms1.isEmpty().
offset: 1137
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts.sc
text:
```scala
import scala.language.implicitConversions
import $file.rexp, rexp._

type Marks = List[String]

// shifts function 
def shifts(ms: Marks, r: Rexp) : Marks = 
  if(ms.length >= 20){
    println(s"ms=${ms.length}")
  }
  r match {
      case ZERO => Nil
      case ONE => Nil
      case CHAR(c) => for (m <- ms; if m != "" && m.head == c) yield m.tail
      case ALT(r1, r2) => (shifts(ms, r1) ::: shifts(ms, r2))
      case SEQ(r1, r2) => {
        val ms1 = shifts(ms, r1).prune2
        (nullable(r1), nullable(r2)) match {
          case (true, true) =>  (shifts((ms1 ::: ms), r2) ::: ms1)
          case (true, false) => shifts((ms1 ::: ms), r2) 
          case (false, true) => (shifts(ms1, r2) ::: ms1)
          case (false, false) => shifts(ms1, r2)
        }
      }
      case STAR(r) => {
        //println(s"ms=${ms.length}")
        val ms1 = shifts(ms, r)
        if(ms1.isEmpty) Nil 
        else
        (ms1 ::: shifts(ms1, STAR(r)))
      }
      case NTIMES(r,n) =>
        if(n==0) Nil
        else if(n==1) shifts(ms,r)
        else{
          val ms1 = shifts(ms,r)
          if(m@@s1.isEmpty) ms1
          else
              if(nullable(r)) ms1 ::: shifts(ms1,NTIMES(r,n-1))
              else shifts(ms1,NTIMES(r,n-1))
        }

}
  

// the main matching function 
def matcher(r: Rexp, s: String) : Boolean = {
  if (s == "") nullable(r)
  else
    {
      val ms=  shifts(List(s), r)
      //println(ms)
      ms.exists(_ == "")
    }
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
  val r = %( "a" | "a" )
  val s = "a" * 4
  println("=string=")
  println(s)
  println(matcher(r,s))
}

@main
def test2() = {
  println("=====Test====")
  val r = %( %( "a" ) | %( "aa" ) | %( "aaa" ) | %( "aaaa" ) | %( "aaaaa" ) )
  val s = "a" * 20
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

empty definition using pc, found symbol in pc: isEmpty.