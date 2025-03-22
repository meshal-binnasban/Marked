error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/play_point3.sc:229
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/play_point3.sc
empty definition using pc, found symbol in pc: 
semanticdb not found
|empty definition using fallback
non-local guesses:
	 -Rexp.SEQ.
	 -Rexp.SEQ#
	 -Rexp.SEQ().
	 -VALUE.SEQ.
	 -VALUE.SEQ#
	 -VALUE.SEQ().
	 -SEQ.
	 -SEQ#
	 -SEQ().
	 -scala/Predef.SEQ.
	 -scala/Predef.SEQ#
	 -scala/Predef.SEQ().

Document text:

```scala
import scala.language.implicitConversions
import os.size

enum Rexp {
  case ZERO 
  case ONE 
  case CHAR(c: Char , bs:List[Int]=List())
  case POINT(r: Rexp, tag:List[Int]=List(), bs:List[Int]=List())
  case ALT(r1: Rexp, r2: Rexp, bs:List[Int]=List()) 
  case SEQ(r1: Rexp, r2: Rexp, bs:List[Int]=List()) 
  case STAR(r: Rexp, bs:List[Int]=List()) 
  case NTIMES(r: Rexp, n: Int , counter: Int = 0, bs:List[Int]=List())
}
import Rexp._

def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_,bs) => false
  case POINT(r,tag,bs) => nullable(r)
  case ALT(r1, r2,bs) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2,bs) => nullable(r1) && nullable(r2)
  case STAR(_,bs) => true
  case NTIMES(r, n,counter,bs) => if (n == 0) true else nullable(r)
}

def fin(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => false
  case CHAR(_,bs) => false 
  case POINT(CHAR(_,b),tag,bs) =>  
    if (tag.head > 0) true else false 
  case ALT(r1, r2,bs) => fin(r1) || fin(r2)
  case SEQ(r1, r2,bs) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r,bs) => fin(r)
  case NTIMES(r, n,counter,bs) => counter == n && fin(r)
  case POINT(r,tag,bs) => fin(r) //?
}

//shift char with position
def shift(m: Boolean, re: Rexp, cp: (Char, Int)) : Rexp = {
  val (c, pos) = cp
  re match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d,bs) => if(m && d == c) 
                    POINT(CHAR(d), List(pos+1),bs) 
                  else CHAR(d,bs)

  case POINT(CHAR(d,b),tag,bs) => if(m && d == c)
                               POINT(CHAR(d), pos+1 ::tag ,bs) 
                             else POINT(CHAR(d), 0::tag,bs)

  case ALT(r1, r2,bs) => ALT(shift(m, r1, cp), shift(m, r2, cp) ,bs) 
  case SEQ(r1, r2,bs) =>
    SEQ(shift(m, r1, cp), shift((m && nullable(r1)) || fin(r1), r2, cp),bs)
  case STAR(r,bs) => 
    //bs ++ List(0)
    STAR(shift(m || fin(r), r, cp),bs)
  //case POINT(NTIMES(r, n,counter)) => NTIMES(r, n,counter)
  case POINT(NTIMES(r, n,counter,b),tag , bs) => 
    POINT(NTIMES(r, n,counter,b),tag:+pos,bs) // maybe b instead of bs
  case NTIMES(r, n,counter,bs) => 
   // println(s"NTIMES: ${matchCount(re)}")
    if (counter == n) re
      else{
        if (m || fin(r)) NTIMES(shift(m || fin(r), r, cp), n, counter+1 , bs)
        else NTIMES(shift(false, r, cp), n, counter,bs)       
        } // if shifted r is final, wrap in point? didn't work
} //end match r
}

def mat(r: Rexp, s: List[Char]): Rexp = s match {
  case Nil => r
  case c :: cs =>
    cs.zipWithIndex.foldLeft(shift(true, r, (c, 0))) {
      case (currRexp, (c, i)) =>
        shift(false, currRexp, (c, i + 1)) 
    }
}

def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s))

def matcher2(r: Rexp, s: List[Char]) : Rexp =
  if (s == Nil)
     if(nullable(r)) r else ZERO 
     else mat(r, s)

// testing bitcodes
@main
def test1() = {
    val rexp = intern(("a" | "b" ))

    println("=============== Test ===============")
    val s="b".toList
    println(s"String: $s\n")
    val finReg=matcher2(rexp, s)
    println(s"Original size=${size(rexp)} Result= ${fin(finReg)} \n")

    for (i <- s.indices) {
    println(s"${i + 1}- =shift ${s(i)}=")
    val sPart = s.take(i + 1)
    println(pp(mat(rexp, sPart)))
    }

    println("\n=============== Final Reg ===============n")
    println(s"Size=${size(finReg)} , Tree= \n ${pp(finReg)}\n")
    println("\n=============== bitcodes ===============n")

    val mkepsValue = mkeps(finReg)
    println(s"mkeps= $mkepsValue")

    println("\n=============== popPoints ===============n")
    val inputLength = s.length        
    val stages = traverseStages(finReg, inputLength)
    stages.reverse.foreach(stage => println( pp(stage))) 

}

//testing popPoints
@main
def test2() = {
  val rexp = STAR("a" | "b" )

  println("=============== Test ===============")
  val s="abba".toList
  println(s"String: $s\n")
  val finReg=matcher2(rexp, s)
  println(s"Original size=${size(rexp)} Result= ${fin(finReg)} \n")


  for (i <- s.indices) {
  println(s"${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(rexp, sPart)))
  }

  println("\n=============== Final Reg ===============n")
  println(s"Size=${size(finReg)} , Tree= \n ${pp(finReg)}\n")
  //println(s"Raw Final Reg= ${finReg} size= ${size(finReg)}")
  println("\n=============== popPoints ===============n")
  val inputLength = s.length        
  val stages = traverseStages(finReg, inputLength)
  stages.reverse.foreach(stage => println( pp(stage))) 

}

//testing sequences, experimented with different sequences stuctures
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

// NTIMES test
@main
def test4() = {
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
  println(s"Points and Regular expressions")
  val extracted = extractPoints(finReg2) 
  extracted.foreach { 
    case (r, point) =>
        println(s"r = $r and point = $point")
    }

}

enum VALUE {
  case ZEROV
  case ONEV
  case CHARV(c: Char)
  case UNMARKED(s:String)
  case SEQV(v1: VALUE, r2: VALUE )
  case LEFT(v: VALUE)
  case RIGHT(v: VALUE)
  case STARV(vs: List[VALUE])
}
import VALUE._

def decode(r: Rexp, bs: List[Int]): (VALUE, List[Int]) = r match {
  case ONE => (ONEV, bs) // not sure this should be included
  case CHAR(c, _) => (CHARV(c), bs) // (2) decode (c) bs = (Char(c), bs)
  case ALT(r1, r2,_) => bs match {
    case 0 :: bs1 => // (3) decode (r1 + r2) 0 :: bs =  (Left(v), bs')  where decode r1 bs => v,bs' 
        val (v, bsp) = decode(r1, bs1)
        (LEFT(v), bsp) 
    case 1 :: bs1 => // (4) decode (r1 + r2) 1 :: bs = (Right(v), bs') where decode r2 bs => v,bs'
        val (v, bsp) = decode(r2, bs1)
        (RIGHT(v), bsp)
    case x =>
      println(s"ALTb bs: $bs and x=$x") 
      (ZEROV, bs) // in case of something else, may need to remove it but just incase
  }

  case SEQ(r1, r2,_) =>  // (5) decode (r1 · r2) bs = (Seq(v1, v2), bs3) where decode r1 bs => v1,bs2 and decode r2 bs2 =>v2,bs3 
    val (v1, bs2) = decode(r1, bs)
    val (v2, bs3) = decode(r2, bs2)
    println(s"SEQb bs: $bs and r1=$r1 and r2=$r2 and bs2=$bs2 and bs3=$bs3")
    (SEQV(v1, v2), bs3) 

  case STAR(r) => bs match {
    case 1 :: bs1 => 
      (STARV(List()), bs1) // Correctly terminate recursion for STAR

    case 0 :: bs1 =>   
      val (v, bs2) = decode(r, bs1)
      val (STARV(vs), bsv) = decode(STAR(r), bs2) 
      (STARV(v :: vs), bsv) 

    case _ => 
      (STARV(List()), bs) // Edge case: No matches in STAR
  
    }// end of match r

}




def mkeps(r: Rexp): List[Int] = r match {
    case ONE => List() 
    case CHAR(_, bs) => List() 
    case POINT(CHAR(_, b),tag, bs) => bs
    case POINT(r,tag , bs) => bs
    case ALT(r1, r2, bs) =>
        if (fin(r1))  bs ++ mkeps(r1) 
        else if (fin(r2)) bs ++ mkeps(r2) 
        else List() //bs
    case SEQ(r1, r2, bs) =>
    if (fin(r1) && nullable(r2)) bs ++ mkeps(r1) ++ mkeps(r2) 
    else if (fin(r2)) bs ++  mkeps(r2)
    else List() //bs
    case STAR(r, bs) =>
        //r match case point add zero else 1 ? also tag?
        if (fin(r)) bs ++ mkeps(r) ++ List(1) 
        else List() 
    case NTIMES(r, n, counter, bs) =>
        if (counter == n && fin(r)) bs ++ mkeps(r) 
        else List() //bs
    case ZERO => List() 
}

def fuse(cs: List[Int], r: Rexp): Rexp = r match {
    case ZERO => ZERO
    case ONE => ONE
    case CHAR(c,bs) => CHAR(c, cs ++ bs)
    case POINT(r, tag, bs) => POINT(r, tag, cs ++ bs)
    case ALT(r1, r2, bs) => ALT(r1, r2, cs ++ bs)
    case SEQ(r1, r2, bs) => SEQ(r1, r2, cs ++ bs)
    case STAR(r, bs) => STAR(r, cs ++ bs)
    case NTIMES(r, n, counter, bs) => NTIMES(fuse(cs, r), n, counter, cs ++ bs)
}

def intern(r: Rexp) : Rexp = r match{
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(c,_) => CHAR(c,List())
  case ALT(r1, r2,bs) => 
    ALT(fuse(List(0), intern(r1)), fuse(List(1), intern(r2)), List())
  case SEQ(r1, r2,bs) => SEQ(intern(r1),intern(r2),List())
  case STAR(r,bs) => STAR(intern(r),List())
  case NTIMES(r, n,counter,bs) => NTIMES(intern(r),n,counter, List())
}


def popPoints(r: Rexp): (Rexp, Rexp) = r match {
  case ZERO => (ZERO, ZERO)
  case ONE  => (ONE, ONE)
  case CHAR(c,bs) => (CHAR(c), CHAR(c)) // Return the same character for both current and next state becaue it doesnt have marks or points
  case POINT(inner, tags,bs) =>
    val (currentInner, nextInner) = popPoints(inner)
    if (tags.isEmpty) {
      // No marker left: return the inner result for both current and next state.
      (currentInner, nextInner)
    } else {
      // Build the updated next state by removing the top marker.
      val updatedNext = POINT(inner, tags.tail)
      tags.head match {
        case 0 =>
          // If the top marker is 0, current stage is unmarked: return the inner result.
          (currentInner, updatedNext)
        case x =>
          // Otherwise, current stage is marked: wrap the inner result with a POINT having tag x or maybe use flag 1 as positive or marked.
          (POINT(currentInner, List(x)), updatedNext)
      }
    }
  case ALT(r1, r2,bs) =>
    val (curr1, next1) = popPoints(r1)
    val (curr2, next2) = popPoints(r2)
    (ALT(curr1, curr2), ALT(next1, next2))
  
  case SEQ(r1, r2,bs) =>
    val (curr1, next1) = popPoints(r1)
    val (curr2, next2) = popPoints(r2)
    (SEQ(curr1, curr2), SEQ(next1, next2))
  
  case STAR(inner,bs) =>
    val (curr, next) = popPoints(inner)
    (STAR(curr), STAR(next))
  
  case NTIMES(inner, n, counter,bs) =>
    val (curr, next) = popPoints(inner)
    (NTIMES(curr, n, counter,bs), NTIMES(next, n, counter,bs))
}

def traverseStages(r: Rexp, inputLength: Int): List[Rexp] = {
  def loop(state: Rexp, stages: List[Rexp], remaining: Int): List[Rexp] = {
    if (remaining == 0) stages
    else {
      val (currentStage, nextState) = popPoints(state)
      loop(nextState, stages :+ currentStage, remaining - 1)
    }
  }
  loop(r, List.empty, inputLength)
}

def traverseStages2(r: Rexp, inputLength: Int): List[Rexp] = {
  val (stages, _) = (0 until inputLength).foldLeft((List[Rexp](), r)) { case ((acc, state), _) =>
    val (currentStage, nextState) = popPoints(state)
    (acc :+ currentStage, nextState)
  }
  stages
}


def extractPoints(r: Rexp): List[(Rexp, Int)] = r match {
  case POINT(r, pos,_) => pos.filter(_ != 0).map(p => (r, p-1)).reverse
  case ALT(r1, r2,_)   => extractPoints(r1) ++ extractPoints(r2)
  case SEQ(r1, r2,_)   => extractPoints(r1) ++ extractPoints(r2)
  case STAR(r,_)       => extractPoints(r)
  case NTIMES(r, _, _,_) => extractPoints(r)
  case _             => List()
}

def matchCount(r: Rexp): Int = r match {
  case ZERO => 0
  case ONE => 0
  case CHAR(_,_) => 1
  case POINT(r,tag,_) => matchCount(r)
  case ALT(r1, r2,_) => math.max(matchCount(r1), matchCount(r2)) // Return the max of both alternatives
  case SEQ(r1, r2,_) => matchCount(r1) + matchCount(r2) // Sum both sequences
 // case STAR(r) => Int.MaxValue // STAR repeats infinitely, set max possible number
  case NTIMES(r, n, _,_) => n * matchCount(r) // Multiply by `n`
}

def size(r: Rexp) : Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_,_) => 1
  case ALT(r1, r2,_) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2,_) => 1 + size(r1) + size(r2)
  case STAR(r,_) => 1 + size(r)
  case NTIMES(r,n,counter,_) => 1 + size(r) 
  case POINT(r,tag,_) => 1 + size(r)}

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
  case CHAR(c,bs) => s"$c bs=$bs\n"
  case POINT(CHAR(c,b),tag,bs) => if(tag.head >0) s"•$c {bs=$bs , charb=$b  }" else s"$c bs=$bs charb=$b\n" //tag=$tag
  case ALT(r1, r2,bs) => s"ALT {bs=$bs}\n" ++ pps(r1, r2)
  case SEQ(r1, r2,bs) => s"SEQ {bs=$bs}\n" ++ pps(r1, r2)
  case STAR(r,bs) => "STAR {bs=$bs}\n" ++ pps(r)
  case NTIMES(r, n,counter,bs) => 
    s"NTIMES{$n} {counter=$counter bs=$bs}\n" ++ pps(r)
  case POINT(NTIMES(r, n, counter,b),tag,bs) => 
    s"• NTIMES {$n} {counter=$counter b=$bs , bs=$bs}\n" ++ pps(r) 
  case POINT(r,tag,bs) =>pp(r)
}
def pps(es: Rexp*) = indent(es.map(pp))

```

#### Short summary: 

empty definition using pc, found symbol in pc: 