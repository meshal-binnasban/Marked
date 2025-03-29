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
  case CHAR(d,marked,tags) => 
    if(m && d == c) 
    CHAR(d,true, pos+1 :: tags)
    else CHAR(d,false, 0 :: tags)
  case ALT(r1, r2) => ALT(shift(m, r1, cp), shift(m, r2, cp)) 
  case SEQ(r1, r2) => SEQ(shift(m, r1, cp), shift((m && nullable(r1)) || fin(r1), r2, cp))
  case STAR(r) => STAR(shift(m || fin(r), r, cp))
  case NTIMES(r, n,counter) => if (counter == n) re else{
        if (m || fin(r)) NTIMES(shift(m || fin(r), r, cp), n, counter+1)
        else NTIMES(shift(false, r, cp), n, counter)       
        }  } }

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

  println("\n=== New PopPoints ===\n")
  val stages=collectStages(finReg)
  stages.foreach(stage => println(pp(stage)))

  println("\n=== Testing popPoints with shift ===\n")
  val r=STAR("a" | "b" | "c" )
  val shiftedR=shift(true,r,('a',0))
  val shiftedRTwice=shift(true,shiftedR,('b',1))
  val shiftedThrice=shift(true,shiftedRTwice,('c',2))
  println(r)
  println(s"original r=\n${pp(r)}")
  println(s"1- Shifted with 'a', r=\n${pp(shiftedR)}")
  println(s"2- Shifted with 'b', r=\n${pp(shiftedRTwice)}")
  println(s"3- Shifted with 'c', r=\n${pp(shiftedThrice)}")
  val popedOnce=popPoints2(shiftedThrice)
  val popedTwice=popPoints2(popedOnce)
  val popedThrice=popPoints2(popedTwice)
  val finalPop=popPoints2(popedThrice)
  println(s"1- popedOnce  r=\n${pp(popedOnce)}")
  println(s"2- popedTwice  r=\n${pp(popedTwice)}")
  println(s"3- popedThrice  r=\n${pp(popedThrice)}")
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
// testing the evil regular expression
@main
def test3() = {
  for (i <- 0 to 7000000 by 500000) {
  
  println(f"$i: ${time_needed(2, matcher2(EVIL2, ("a" * i).toList))}%.5f")

  }
}


def popPoints2(r: Rexp): Rexp = r match {
  case ZERO => ZERO
  case ONE  => ONE
    /* case CHAR(c, _, tags) =>
        tags match {
            case Nil => CHAR(c, false, Nil) 
            case h :: tail =>CHAR(c, h > 0, tail)
            } */
  case CHAR(c, _, tags) => tags match {
    case Nil => CHAR(c, false, Nil)
    case _ :: next :: rest => CHAR(c, next > 0, next :: rest)
    case _ :: Nil => CHAR(c, false, Nil)
    }         
  case ALT(r1, r2) => ALT(popPoints2(r1),popPoints2(r2))
  case SEQ(r1, r2) => SEQ(popPoints2(r1), popPoints2(r2))
  case STAR(inner) => STAR(popPoints2(inner))
  case NTIMES(inner, n, counter) =>NTIMES(popPoints2(inner), n, counter)
}

// helper function to check if any CHAR in the regex still has tags left
def containsTags(r: Rexp): Boolean = r match {
  case CHAR(_, _, tags) => tags.nonEmpty
  case ALT(r1, r2)      => containsTags(r1) || containsTags(r2)
  case SEQ(r1, r2)      => containsTags(r1) || containsTags(r2)
  case STAR(r1)         => containsTags(r1)
  case NTIMES(r1, _, _) => containsTags(r1)
  case _                => false
}

// repeatedly calling popPoints2 until there is no tags left 
def collectStages(finReg: Rexp): List[Rexp] = {
  def loop(current: Rexp, stages: List[Rexp]): List[Rexp] = {
    if (!containsTags(current)) stages
    else {
      val next = popPoints2(current)
      loop(next, stages :+ next)
    }
  }

  //val first = popPoints2(finReg)
  loop(finReg, List())
}




def extractPoints(r: Rexp): List[(Rexp, Int)] = r match {
  case CHAR(c,marked,tags) => tags.filter(_ != 0).map(p => (r, p-1)).reverse
  case ALT(r1, r2)   => extractPoints(r1) ++ extractPoints(r2)
  case SEQ(r1, r2)   => extractPoints(r1) ++ extractPoints(r2)
  case STAR(r)       => extractPoints(r)
  case NTIMES(r, _, _) => extractPoints(r)
  case _             => List()
}

def size(r: Rexp) : Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_,_,_) => 1
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
  case CHAR(c,marked,tags) => if(marked) s"•$c tags={$tags}\n" else s"$c tags={$tags}\n"
  case ALT(r1, r2) => s"ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => s"SEQ\n" ++ pps(r1, r2)
  case STAR(r) => "STAR\n" ++ pps(r)
  case NTIMES(r, n,counter) => 
    s"NTIMES{$n}{fin r=${fin(r)}, null r=${nullable(r)} counter=$counter}\n" ++ pps(r)
    s"• NTIMES {$n}{fin r=${fin(r)}, null r=${nullable(r)}, counter=$counter}\n" ++ pps(r) 
}
def pps(es: Rexp*) = indent(es.map(pp))
