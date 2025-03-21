error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/play_point2.sc:77
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/play_point2.sc
empty definition using pc, found symbol in pc: 
semanticdb not found
|empty definition using fallback
non-local guesses:
	 -Rexp.NTIMES.
	 -Rexp.NTIMES#
	 -Rexp.NTIMES().
	 -NTIMES.
	 -NTIMES#
	 -NTIMES().
	 -scala/Predef.NTIMES.
	 -scala/Predef.NTIMES#
	 -scala/Predef.NTIMES().

Document text:

```scala
import scala.language.implicitConversions

enum Rexp {
  case ZERO 
  case ONE 
  case CHAR(c: Char)
  case POINT(r: Rexp, tag:List[Int])
  case ALT(r1: Rexp, r2: Rexp) 
  case SEQ(r1: Rexp, r2: Rexp) 
  case STAR(r: Rexp) 
  case NTIMES(r: Rexp, n: Int , counter: Int = 0)
}
import Rexp._

def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case POINT(r,tag) => nullable(r)
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, n,counter) => if (n == 0) true else nullable(r)
}

def fin(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => false
  case CHAR(_) => false 
  case POINT(CHAR(_),tag) =>  true   //if tag > 0 true 
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
  case NTIMES(r, n,counter) => counter == n && fin(r)
  case POINT(r,tag) => fin(r) //?
}

def shift(m: Boolean, re: Rexp, cp: (Char, Int)) : Rexp = {
  val (c, pos) = cp

  re match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if(m && d == c) POINT(CHAR(d), List(pos)) else CHAR(d)

  case POINT(CHAR(d),tag) => if(m && d == c) POINT(CHAR(d), tag :+ pos) else CHAR(d)

  case ALT(r1, r2) => ALT(shift(m, r1, cp), shift(m, r2, cp)) 
  case SEQ(r1, r2) =>
    if (m && nullable(r1))
      {
        ALT(
            SEQ(shift(m, r1, cp), shift(fin(r1), r2, cp)) // [] as bs for the SEQ
                , 
            shift(true, r2, cp))
      }
      else{
        SEQ(shift(m, r1, cp), shift(fin(r1), r2, cp))   
      }  
   
   /*
    val r1Shifted = shift(m, r1, cp)      
    if (fin(r1Shifted)) {            
        // then shift r2 (using the same flag as r1 was initially shifted)
        println("here")
        SEQ(r1Shifted, shift(m && nullable(r1) || fin(r1Shifted), r2, cp))
    } else {
    // otherwise, leave r2 as it is.
    SEQ(r1Shifted, r2)
    }
*/
    //SEQ(shift(m, r1, cp), shift((m && nullable(r1)) || fin(r1), r2, cp))
  case STAR(r) => STAR(shift(m || fin(r), r, cp))
  //case POINT(NTIMES(r, n,counter)) => NTIMES(r, n,counter)
  
  case POINT(NTIMES(r, n,counter),tag) => 
    re
  case NTIMES(r, n,counter) => 
   // println(s"NTIMES: ${matchCount(re)}")
    if (counter == n) re
      else{

        val shiftedR=shift(m || fin(r), r, cp)
        if (fin(shiftedR))
            if(counter == n-1) POINT(NTIMES(shiftedR, n, counter+1),List(pos))
            else NTIMES(shiftedR, n, counter + 1)
        else NTIMES(shift(false, r, cp), n, counter)       
        }


}
}


def mat(r: Rexp, s: List[Char]): Rexp = s match {
  case Nil => r
  case c :: cs =>
    cs.zipWithIndex.foldLeft(shift(true, r, (c, 0))) {
      case (currRexp, (c, i)) =>
        shift(false, currRexp, (c, i + 1)) // ✅ Correctly increment position
    }
}


def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s))

def matcher2(r: Rexp, s: List[Char]) : Rexp =
  if (s == Nil)
     if(nullable(r)) r else ZERO 
     else mat(r, s)

@main
def test1() = {

  //val rexp=ALT(NTIMES("a" | "b" , 2), NTIMES("c" | "d" , 2)) working
  //val rexp=NTIMES( NTIMES("a",2) , 1) not working
  val n=2
  val rexp = (NTIMES("a" | ONE, n)) ~ (NTIMES(CHAR('a'), n))

  println("=====Test====")
  val ss="aaaab".toList
  println(s"String: $ss\n")
  println(s"Result= ${matcher(rexp, ss)}\n")

  
  for (i <- ss.indices) {
  println(s"${i + 1}- =shift ${ss(i)}=")
  val sPart = ss.take(i + 1)
  println(pp(mat(rexp, sPart)))
  }
  

  val finReg2=matcher2(rexp, ss)
  println(s"Final Reg Tree= \n ${pp(finReg2)}\n")
  println(s"Raw Final Reg= ${finReg2}")
  println(s" finReg points= ${extractPoints(finReg2)}")
  println(s" finReg points r= ${extractPoints2(finReg2)}")
}

@main
def test2() = {
  println("=====Test====")
  val br1 = SEQ(STAR("a" | ONE), STAR("b"~"b"))

  val s = "aaabb".toList

  println(s"Result= ${matcher(br1, s)} \n")
  val finReg=matcher2(br1, s)
  println(s"Final Reg Tree= \n ${pp(finReg)} \n")
  println(s"Raw Reg= ${finReg}")


}

@main
def test3() = {
  println("=====Test====")
  val rexp = ("a" ~ "b") 
  val s = "ab".toList
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

def extractPoints(r: Rexp): List[Int] = r match {
  case POINT(_, pos) => List(pos)
  case ALT(r1, r2) => extractPoints(r1) ++ extractPoints(r2)
  case SEQ(r1, r2) => extractPoints(r1) ++ extractPoints(r2)
  case STAR(r) => extractPoints(r)
  case NTIMES(r, _, _) => extractPoints(r)
  case _ => List()
}

def extractPoints2(r: Rexp): List[(Rexp, Int)] = r match {
  case POINT(r, pos) => List((r, pos)) // ✅ Store the marked regex and position
  case ALT(r1, r2) => extractPoints2(r1) ++ extractPoints2(r2)
  case SEQ(r1, r2) => extractPoints2(r1) ++ extractPoints2(r2)
  case STAR(r) => extractPoints2(r)
  case NTIMES(r, _, _) => extractPoints2(r)
  case _ => List()
}


def matchCount(r: Rexp): Int = r match {
  case ZERO => 0
  case ONE => 0
  case CHAR(_) => 1
  case POINT(r,tag) => matchCount(r)
  case ALT(r1, r2) => math.max(matchCount(r1), matchCount(r2)) // Return the max of both alternatives
  case SEQ(r1, r2) => matchCount(r1) + matchCount(r2) // Sum both sequences
 // case STAR(r) => Int.MaxValue // STAR repeats infinitely, set max possible number
  case NTIMES(r, n, _) => n * matchCount(r) // Multiply by `n`
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
  case CHAR(c) => s"$c\n"
  case POINT(CHAR(c),tag) => s"•$c\n"
  case ALT(r1, r2) => s"ALT{fin r1=${fin(r1)}, null r1=${nullable(r1)}}\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => s"SEQ{fin r1=${fin(r1)}, null r1=${nullable(r1)}}\n" ++ pps(r1, r2)
  case STAR(r) => "STAR\n" ++ pps(r)
  case NTIMES(r, n,counter) => 
    s"NTIMES{$n}{fin r=${fin(r)}, null r=${nullable(r)} counter=$counter}\n" ++ pps(r)
  case POINT(NTIMES(r, n, counter),tag) => 
    s"• NTIMES {$n}{fin r=${fin(r)}, null r=${nullable(r)}, counter=$counter}\n" ++ pps(r) 
  case POINT(r,tag) =>pp(r)
}
def pps(es: Rexp*) = indent(es.map(pp))

```

#### Short summary: 

empty definition using pc, found symbol in pc: 