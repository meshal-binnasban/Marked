
enum Rexp {
  case ZERO 
  case ONE 
  case CHAR(c: Char)
  case POINT(r: Rexp)
  case ALT(r1: Rexp, r2: Rexp) 
  case SEQ(r1: Rexp, r2: Rexp) 
  case STAR(r: Rexp) 
  case NTIMES(r: Rexp, n: Int , counter: Int = 0)
}
import Rexp._

// some syntax sugar for regexes
import scala.language.implicitConversions

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

def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case POINT(r) => nullable(r)
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, n,counter) => if (n == 0) true else nullable(r)
}


// fin function from the paper
// checks whether a mark is in "final" position
def fin(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => false
  case CHAR(_) => false 
  case POINT(CHAR(_)) => true
  case NTIMES(r, n,counter) => false
    //println("here")
   // if(counter < n && nullable(r)) fin(r) else false
  case POINT(NTIMES(r, n, counter)) => true //if(counter ==n) true else false
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)

}

// shift function from the paper
def shift(m: Boolean, r: Rexp, c: Char) : Rexp = {
  //println(s"mode: $m")
  r match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if(m && d == c) POINT(CHAR(d)) else CHAR(d)
  case POINT(CHAR(d)) => if(m && d == c) POINT(CHAR(d)) else CHAR(d)
  case ALT(r1, r2) => ALT(shift(m, r1, c), shift(m, r2, c)) 
  case SEQ(r1, r2) =>
    SEQ(shift(m, r1, c), shift((m && nullable(r1)) || fin(r1), r2, c))
   
   /*
    //println(s"cond: ${m && nullable(r1)}")
    if (m && nullable(r1))
    then {
      //println("if case")
      ALT(SEQ(shift(m, r1, c), shift(fin(r1), r2, c)), shift(true, r2, c))
    }
    else {
      //println("else case")
      SEQ(shift(m, r1, c), shift(fin(r1), r2, c))   
    } 

    */ 
  case STAR(r) => STAR(shift(m || fin(r), r, c))

  case POINT(NTIMES(r, n,counter)) => NTIMES(r, n,counter)
  case NTIMES(r, n,counter) => 
    if (counter == n) NTIMES(r, n,counter) else 
        if (m || fin(r)) {
            val nt=NTIMES(shift(m || fin(r), r, c), n , counter + 1)
            if (counter + 1 == n && fin(r))
                POINT(nt) 
                else 
                    nt
        }
        else NTIMES(shift(false, r, c), n)
}
}

def mat(r: Rexp, s: List[Char]) : Rexp = s match {
  case Nil => r
  case c::cs => cs.foldLeft(shift(true, r, c))((r, c) => shift(false, r, c))
}

def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s))

def matcher2(r: Rexp, s: List[Char]) : Rexp =
  if (s == Nil)
     if(nullable(r)) r else ZERO 
     else mat(r, s)

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
  case CHAR(c) => s"$c\n"
  case POINT(CHAR(c)) => s"•$c\n"
  case ALT(r1, r2) => s"ALT{fin r1=${fin(r1)}, null r1=${nullable(r1)}}\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => s"SEQ{fin r1=${fin(r1)}, null r1=${nullable(r1)}}\n" ++ pps(r1, r2)
  case STAR(r) => "STAR\n" ++ pps(r)
  case NTIMES(r, n,counter) => 
    s"NTIMES{$n}{fin r=${fin(r)}, null r=${nullable(r)} counter=$counter}\n" ++ pps(r)
  case POINT(NTIMES(r, n, counter)) => 
    s"• NTIMES {$n}{fin r=${fin(r)}, null r=${nullable(r)}, counter=$counter}\n" ++ pps(r) 
}
def pps(es: Rexp*) = indent(es.map(pp))

@main
def test1() = {

  //val rexp=ALT(NTIMES("a" | "b" , 2), NTIMES("c" | "d" , 2)) working
  //val rexp=NTIMES( NTIMES("a",2) , 1) not working
  val n=2
  val rexp = (NTIMES("a" | ONE, n)) ~ (NTIMES(CHAR('a'), n))

  println("=====Test====")
  val ss="aaaa".toList
  println(s"String: $ss\n")
  println(s"Result= ${matcher(rexp, ss)}\n")

  for (i <- ss.indices) {
  println(s"${i + 1}- =shift ${ss(i)}=")
  val sPart = ss.take(i + 1)
  println(pp(mat(rexp, sPart)))
  }
  

  val finReg2=matcher2(rexp, ss)
  println(s"Final Reg Tree= \n ${pp(finReg2)}\n")
  println(s"Raw Reg= ${finReg2}")
}

@main
def test2() = {
  println("=====Test====")
  val br1 = SEQ(ALT("a", "ab"), ALT("bc", "c"))
  val s = "abc".toList

  println(s"Result= ${matcher(br1, s)} \n")
  val finReg=matcher2(br1, s)
  println(s"Final Reg Tree= \n ${pp(finReg)} \n")
  println(s"Raw Reg= ${finReg}")
}

@main
def test3() = {
  println("=====Test====")
  val rexp = ("a" ~ "b") ~ "c"
  val s = "abc".toList
  println(s"start: $rexp")
  println("=============\n")

  for (n <- (1 to s.length)) {
    val sl = s.slice(0, n)
    println(s"shift: ${sl.last}")
    println(pp(mat(rexp, sl)))
  }
  println(s"Result= ${matcher(rexp, s)}")
  val finReg=matcher2(rexp, s)
  println(s"Final Reg Tree= \n ${pp(finReg)}\n")
  println(s"Raw Reg= ${finReg}")
}

@main
def test4() = {
  println("=====Test====")
  
  val rexp = (NTIMES("a" | ONE, 2)) ~ (NTIMES(CHAR('a'), 2))

  val s = "aaaa".toList
  println(matcher(rexp, s))
  
  println(s"Result= ${matcher(rexp, s)}")
  val finReg=matcher2(rexp, s)
  println(s"Final Reg Tree= \n ${pp(finReg)}\n")
  println(s"Raw Reg= ${finReg}")

  println(s"\n\n  ============================ \n\n")

}
