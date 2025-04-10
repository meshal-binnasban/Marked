error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/play_tags.sc:40
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/play_tags.sc
empty definition using pc, found symbol in pc: 
semanticdb not found
|empty definition using fallback
non-local guesses:
	 -Rexp.CHAR.
	 -Rexp.CHAR#
	 -Rexp.CHAR().
	 -CHAR.
	 -CHAR#
	 -CHAR().
	 -scala/Predef.CHAR.
	 -scala/Predef.CHAR#
	 -scala/Predef.CHAR().

Document text:

```scala
import scala.language.implicitConversions
import os.size

enum Rexp {
  case ZERO 
  case ONE 
  case CHAR(c: Char, marked: Boolean = false, tags: List[Int] = List())
  case ALT(r1: Rexp, r2: Rexp) 
  case SEQ(r1: Rexp, r2: Rexp) 
  case STAR(r: Rexp) 
  case NTIMES(r: Rexp, n: Int , counter: Int = 0)
}
import Rexp._

def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_,_,_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, n,counter) => if (n == 0) true else nullable(r)
}

def fin(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => false
  case CHAR(c,marked,tags) => marked 
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
  case NTIMES(r, n,counter) => counter == n && fin(r)
}

//shift char with position
def shift(m: Boolean, re: Rexp, cp: (Char, Int)) : Rexp = {
  val (c, pos) = cp
  re match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d,marked,tags) => if(m && d == c) 
                    POINT(CHAR(d), List(pos+1)) 
                  else CHAR(d)

  case POINT(CHAR(d),tag) => if(m && d == c)
                               POINT(CHAR(d), pos+1 ::tag ) 
                             else POINT(CHAR(d), 0::tag)

  case ALT(r1, r2) => ALT(shift(m, r1, cp), shift(m, r2, cp)) 
  case SEQ(r1, r2) => // add seq normal
    if (m && nullable(r1))
      {
        ALT(
            SEQ(shift(m, r1, cp), shift(fin(r1), r2, cp)) 
                , 
            shift(true, r2, cp))
      }
      else{
        SEQ(shift(m, r1, cp), shift(fin(r1), r2, cp))   
      }  
    //SEQ(shift(m, r1, cp), shift((m && nullable(r1)) || fin(r1), r2, cp))
  case STAR(r) => STAR(shift(m || fin(r), r, cp))
  
  //case POINT(NTIMES(r, n,counter)) => NTIMES(r, n,counter)
  case POINT(NTIMES(r, n,counter),tag) => 
    POINT(NTIMES(r, n,counter),tag:+pos)
  case NTIMES(r, n,counter) => 
   // println(s"NTIMES: ${matchCount(re)}")
    if (counter == n) re
      else{
        if (m || fin(r)) NTIMES(shift(m || fin(r), r, cp), n, counter+1)
        else NTIMES(shift(false, r, cp), n, counter)       
        } // if shifted r is final, wrap in point? didn't work
}
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



@main
def test1() = {
  println("===== Testing New PopPoints =====")
  
  val rexp = STAR("a" | "b" )
  val s="abba".toList
  println(s"String: $s\n")
  val finReg=matcher2(rexp, s)
  println(s"Original size=${size(rexp)} Result= ${fin(finReg)} Final Size=${size(finReg)} \n")
  println(s"finReg=${finReg}")

  println("=== Old PopPoints ===")
  val inputLength = s.length        
  val stages = traverseStages2(finReg, inputLength)
  stages.foreach(stage => println( pp(stage)))
  println("=== New PopPoints ===")
  val regAfterPop=popPoints2(finReg)
  println(s"Testing popoints2 after popPoints2=${regAfterPop}")
  println(pp(regAfterPop))
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

  val inputLength = s.length        
  val stages = traverseStages2(finReg, inputLength)
  stages.foreach(stage => println( pp(stage)))

  println(s"Points and Part expressions") 
  val extracted = extractPoints(finReg) 
  extracted.sortBy(_._2).foreach { 
    case (r, point) =>
        println(s"r = $r and point = $point")
    }



}

val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}
@main
def test3() = {
  for (i <- 0 to 7000000 by 500000) {
  
  println(f"$i: ${time_needed(2, matcher2(EVIL2, ("a" * i).toList))}%.5f")

  }
}

// recieves a regular expression, pops the head of the list to represent the marks and keeps the tail of the list in the regular expression 
//if done repeatdly it will return the last final regular expression
def popPoints2(r: Rexp): Rexp = r match {
  case ZERO => ZERO
  case ONE  => ONE
  case CHAR(c) => CHAR(c) 
  case POINT(inner, tags) =>
    tags match {
      case Nil => inner 
      case 0 :: tail => 
        if (tail.isEmpty) inner
        else POINT(inner, tail)
      case _ :: tail =>
        POINT(inner, tail)
    }

  case ALT(r1, r2) => ALT(popPoints2(r1),popPoints2(r2))
  case SEQ(r1, r2) => SEQ(popPoints2(r1), popPoints2(r2))
  case STAR(inner) => STAR(popPoints2(inner))
  case NTIMES(inner, n, counter) =>NTIMES(popPoints2(inner), n, counter)
}

def extractPoints(r: Rexp): List[(Rexp, Int)] = r match {
  case POINT(r, pos) => pos.filter(_ != 0).map(p => (r, p-1)).reverse
  case ALT(r1, r2)   => extractPoints(r1) ++ extractPoints(r2)
  case SEQ(r1, r2)   => extractPoints(r1) ++ extractPoints(r2)
  case STAR(r)       => extractPoints(r)
  case NTIMES(r, _, _) => extractPoints(r)
  case _             => List()
}

def popPoints(r: Rexp): (Rexp, Rexp) = r match {
  case ZERO => (ZERO, ZERO)
  case ONE  => (ONE, ONE)
  case CHAR(c) => (CHAR(c), CHAR(c)) // Return the same character for both current and next state becaue it doesnt have marks or points

  case POINT(inner, tags) =>
    val (currentInner, nextInner) = popPoints(inner)
    if (tags.isEmpty) {
      (currentInner, nextInner)
    } else {
      val updatedNext = POINT(inner, tags.tail)
      tags.head match {
        case 0 =>
          (currentInner, updatedNext)
        case x =>
          (POINT(currentInner, List(x)), updatedNext)
      }
    }
  case ALT(r1, r2) =>
    val (curr1, next1) = popPoints(r1)
    val (curr2, next2) = popPoints(r2)
    (ALT(curr1, curr2), ALT(next1, next2))
  
  case SEQ(r1, r2) =>
    val (curr1, next1) = popPoints(r1)
    val (curr2, next2) = popPoints(r2)
    (SEQ(curr1, curr2), SEQ(next1, next2))
  
  case STAR(inner) =>
    val (curr, next) = popPoints(inner)
    (STAR(curr), STAR(next))
  
  case NTIMES(inner, n, counter) =>
    val (curr, next) = popPoints(inner)
    (NTIMES(curr, n, counter), NTIMES(next, n, counter))
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
  val (stages, _) = (0 until inputLength).foldLeft((List[Rexp](), r)) { 
    case ((acc, state), _) =>
      val (currentStage, nextState) = popPoints(state)
      (acc :+ currentStage, nextState)
  }
  stages
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

def size(r: Rexp) : Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_) => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r) => 1 + size(r)
  case NTIMES(r,n,counter) => 1 + size(r) 
  case POINT(r,tag) => 1 + size(r)}

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
  case POINT(CHAR(c),tag) => if(tag.head >0) s"•$c\n" else s"$c\n"
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