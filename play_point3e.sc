import scala.language.implicitConversions
import os.size

enum Rexp {
  case ZERO 
  case ONE 
  case CHAR(c: Char ,marked:Boolean=false, bs:List[Int]=List())
  case ALT(r1: Rexp, r2: Rexp, bs:List[Int]=List()) 
  case SEQ(r1: Rexp, r2: Rexp, bs:List[Int]=List()) 
  case STAR(r: Rexp, bs:List[Int]=List()) 
}
import Rexp._

def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_,_,_) => false
  case ALT(r1, r2,_) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2,_) => nullable(r1) && nullable(r2)
  case STAR(_,_) => true
}

def fin(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => false
  case CHAR(c,marked,bs) => marked 
  case ALT(r1, r2,_) => fin(r1) || fin(r2)
  case SEQ(r1, r2,_) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r,_) => fin(r)
}

//shift char with position
def shift(m: Boolean, re: Rexp, c: Char) : Rexp = {
  re match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d,marked,bs) => CHAR(d,m && d == c,bs) 
  case ALT(r1, r2,bs) => ALT(shift(m, r1, c), shift(m, r2, c) ,bs) 
  case SEQ(r1, r2,bs) => SEQ(shift(m, r1, c), shift((m && nullable(r1)) || fin(r1), r2, c),bs)
  case STAR(r,bs) => 
    //bs ++ List(0)
    STAR(shift(m || fin(r), r, c),bs ++ mkeps(r) ++  List(0))
  //case POINT(NTIMES(r, n,counter)) => NTIMES(r, n,counter)
} //end match r
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

// testing bitcodes
@main
def test1() = {
    val rexp = intern(STAR( ALT( ALT("a","b") , "c" ) ))

    println("=============== Test ===============")
    val s="abc".toList
    println(s"String: $s\n")
    val finReg=matcher2(rexp, s)
    println(s"Original size=${size(rexp)} Result= ${fin(finReg)} \n")

    /*
    for (i <- s.indices) {
    println(s"${i + 1}- =shift ${s(i)}=")
    val sPart = s.take(i + 1)
    println(pp(mat(rexp, sPart)))
    }
*/
    println("\n=============== Final Reg ===============n")
    println(s"Size=${size(finReg)} , Tree= \n ${finReg}\n")
    println("\n=============== bitcodes ===============n")

    val mkepsValue = mkeps(finReg)
    println(s"mkeps= $mkepsValue")
    val decodeValue=decode(rexp,mkepsValue)
    println(s"decode=$decodeValue")

}

//testing popPoints
@main
def test2() = {

}

//testing sequences, experimented with different sequences stuctures
@main
def test3() = {

}

// NTIMES test
@main
def test4() = {
  
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
  case CHAR(c,marked, bs) => if(marked) (CHARV(c), bs) else (ZEROV, bs) // (2) decode (c) bs = (Char(c), bs)
  case ALT(r1, r2,_) => bs match {
    case 0 :: bs1 => // (3) decode (r1 + r2) 0 :: bs =  (Left(v), bs')  where decode r1 bs => v,bs' 
        val (v, bsp) = decode(r1, bs1)
        (LEFT(v), bsp) 
    case 1 :: bs1 => // (4) decode (r1 + r2) 1 :: bs = (Right(v), bs') where decode r2 bs => v,bs'
        val (v, bsp) = decode(r2, bs1)
        (RIGHT(v), bsp)
    case x =>
      (ZEROV, bs) // in case of something else, may need to remove it but just incase
  }
  case SEQ(r1, r2,_) =>  // (5) decode (r1 · r2) bs = (Seq(v1, v2), bs3) where decode r1 bs => v1,bs2 and decode r2 bs2 =>v2,bs3 
    val (v1, bs2) = decode(r1, bs)
    val (v2, bs3) = decode(r2, bs2)
    (SEQV(v1, v2), bs3) 

  case STAR(r,_) => bs match {
    case 1 :: bs1 => 
      (STARV(List()), bs1) // terminate recursion for STAR
    case 0 :: bs1 =>   
      val (v, bs2) = decode(r, bs1)
      val (STARV(vs), bsv) = decode(STAR(r,bs2), bs2) 
      (STARV(v :: vs), bsv) 
    case _ => 
      (STARV(List()), bs) // Edge case: No matches in STAR
    }// end of match r
}

def mkeps(r: Rexp): List[Int] = r match {
    case ONE => List() 
    case CHAR(_, marked,bs) => if(marked) bs else List()
    case ALT(r1, r2, bs) =>
        if (fin(r1))  bs ++ mkeps(r1) 
        else if (fin(r2)) bs ++ mkeps(r2) 
        else List() //bs
    case SEQ(r1, r2, bs) =>
    if (fin(r1) && nullable(r2)) bs ++ mkeps(r1) ++ mkeps(r2) 
    else if (fin(r2)) bs  ++ mkeps(r2) 
    else List() //bs
    case STAR(r, bs) =>
        //r match case point add zero else 1 ? also tag?
        if (fin(r)) bs ++ mkeps(r) ++ List(1) 
        else List() 
    case ZERO => List() 
}


def fuse(cs: List[Int], r: Rexp): Rexp = r match {
    case ZERO => ZERO
    case ONE => ONE
    case CHAR(c,marked,bs) => CHAR(c,marked, cs ++ bs)
    case ALT(r1, r2, bs) => ALT(r1, r2, cs ++ bs)
    case SEQ(r1, r2, bs) => SEQ(r1, r2, cs ++ bs)
    case STAR(r, bs) => STAR(r, cs ++ bs)
}

def intern(r: Rexp) : Rexp = r match{
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(c,marked,_) => CHAR(c,marked,List())
  case ALT(r1, r2,bs) => 
    ALT(fuse(List(0), intern(r1)), fuse(List(1), intern(r2)), List())
  case SEQ(r1, r2,bs) => SEQ(intern(r1),intern(r2),List())
  case STAR(r,bs) => STAR(intern(r),List())
}

def size(r: Rexp) : Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_,_,_) => 1
  case ALT(r1, r2,_) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2,_) => 1 + size(r1) + size(r2)
  case STAR(r,_) => 1 + size(r)
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



val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

@main
def maintest2() = {
 // for (i <- 0 to 7000000 by 500000) {
 // }
  val i=10000
  println(f"$i: ${time_needed(2, matcher(EVIL2, ("a" * i).toList:+ 'b') ) }%.5f")
  //println(f"$i: ${time_needed(2, decode(EVIL2,mkeps(matcher2(EVIL2, ("a" * i).toList)) )) }%.5f")
  
 
} 

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}