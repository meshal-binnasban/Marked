error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/play_ComplexMarks.sc:62
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/play_ComplexMarks.sc
empty definition using pc, found symbol in pc: 
semanticdb not found
|empty definition using fallback
non-local guesses:
	 -Rexp.Color.
	 -Color.
	 -scala/Predef.Color.

Document text:

```scala
import scala.language.implicitConversions
import os.size

enum Color:
  case GREEN
  case RED

case class Mark(
  marked: Boolean = false,
  bs: List[Int] = List(),
  color: Color = Color.RED
)

enum Rexp {
  case ZERO 
  case ONE 
  case CHAR(c: Char, marked: Mark=Mark())
  case ALT(r1: Rexp, r2: Rexp) 
  case SEQ(r1: Rexp, r2: Rexp) 
  case STAR(r: Rexp) 
  case NTIMES(r: Rexp, n: Int , counter: Int = 0)
}
import Rexp._


def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_,_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, n,counter) => if (n == 0) true else nullable(r)
}

def fin(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => false
  case CHAR(c,marked) => marked.marked 
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
  case NTIMES(r, n,counter) => counter == n && fin(r)
}

//shift char with position
def shift(m: Mark, re: Rexp, c: Char) : Rexp = {
  re match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d,mark) => 
    CHAR(d, Mark(marked = m.marked && d == c, bs = m.bs, color = m.color))
  case ALT(r1, r2) => ALT(shift(m, r1, c), shift(m, r2, c)) 
  case SEQ(r1, r2) => SEQ(shift(m, r1, c), shift(Mark( (m.marked && nullable(r1)) || fin(r1) ,m.bs,m.color), r2, c))
  case STAR(r) => STAR(shift(Mark(m.marked || fin(r),m.bs,m.color), r, c))
  case NTIMES(r, n,counter) => if (counter == n) re else{
        if (m.marked || fin(r)) NTIMES(shift(Mark(m.marked || fin(r),m.bs,m.color), r, c), n, counter+1)
        else NTIMES(shift(Mark(false,m.bs,m.color), r, c), n, counter)       
        }  } }

def mat(r: Rexp, s: List[Char]) : Rexp = s match {
  case Nil => r
  case c::cs => cs.foldLeft(shift(Mark(true,List(),Color.RED), r, c))((r, c) => shift(false, r, c))
}

def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s))

def matcher2(r: Rexp, s: List[Char]) : Rexp =
  if (s == Nil)
     if(nullable(r)) r else ZERO 
     else mat(r, s)


@main
def test1() = {
  println("\n===== Testing New PopPoints =====\n")
  
  val rexp = STAR("a" | "b" | "c" )
  val s="abc".toList
  println(s"String: $s\n")
  val finReg=matcher2(rexp, s)
  println(s"Original size=${size(rexp)} Result= ${fin(finReg)} Final Size=${size(finReg)}")
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(rexp, sPart)))
  }

  println(s"finReg=${finReg}")

  println("\n=== Testing popPoints with shift ===\n")
  val r=STAR("a" | "b" | "c" )
  val shiftedR=shift(true,r,('a'))
  val shiftedRTwice=shift(true,shiftedR,('b'))
  val shiftedThrice=shift(true,shiftedRTwice,('c'))
  println(r)
  println(s"original r=\n${pp(r)}")
  println(s"1- Shifted with 'a', r=\n${pp(shiftedR)}")
  println(s"2- Shifted with 'b', r=\n${pp(shiftedRTwice)}")
  println(s"3- Shifted with 'c', r=\n${pp(shiftedThrice)}")
  
}


@main
def test2() = {
  val rexp = STAR("a" | "b" )

  println("===== Testing Tags =====")
  val s="abba".toList
  println(s"String: $s\n")
  val finReg=matcher2(rexp, s)
  println(s"Original size=${size(rexp)} Result= ${fin(finReg)} \n")


  for (i <- s.indices) {
  println(s"${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(rexp, sPart)))
  }

  println(s"Final Reg Tree= \n ${pp(finReg)}\n")
  println(s"Raw Final Reg= ${finReg} size= ${size(finReg)}")

}

val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}
// testing the evil regular expression
@main
def test3() = {
  for (i <- 0 to 7000000 by 500000) {
  
  println(f"$i: ${time_needed(2, matcher2(EVIL2, ("a" * i).toList))}%.5f")

  }
}


def size(r: Rexp) : Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_,_) => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r) => 1 + size(r)
  case NTIMES(r,n,counter) => 1 + size(r) 
}

// some syntax sugar for regexes
def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
// strings are coerced into Rexps
given Conversion[String, Rexp] = s => charlist2rexp(s.toList)

//val ABCD : Rexp = "abcd"

extension (r: Rexp) {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

// pretty-printing REGs
def implode(ss: Seq[String]) = ss.mkString("\n")
def explode(s: String) = s.split("\n").toList

def lst(s: String) : String = explode(s) match {
  case hd :: tl => implode(" └" ++ hd :: tl.map("  " ++ _))
  case Nil => ""
}

def mid(s: String) : String = explode(s) match {
  case hd :: tl => implode(" ├" ++ hd :: tl.map(" │" ++ _))
  case Nil => ""
}

def indent(ss: Seq[String]) : String = ss match {
  case init :+ last => implode(init.map(mid) :+ lst(last))
  case _ => "" 
}

def pp(e: Rexp) : String = e match {
  case ZERO => "0\n"
  case ONE => "1\n"
  case CHAR(c,marked) => if(marked) s"•$c marks={}\n" else s"$c marks={}\n"
  case ALT(r1, r2) => s"ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => s"SEQ\n" ++ pps(r1, r2)
  case STAR(r) => "STAR\n" ++ pps(r)
  case NTIMES(r, n,counter) => 
    s"NTIMES{$n}{fin r=${fin(r)}, null r=${nullable(r)} counter=$counter}\n" ++ pps(r)
    s"• NTIMES {$n}{fin r=${fin(r)}, null r=${nullable(r)}, counter=$counter}\n" ++ pps(r) 
}
def pps(es: Rexp*) = indent(es.map(pp))

```

#### Short summary: 

empty definition using pc, found symbol in pc: 