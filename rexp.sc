enum VALUE {
    case EMPTY
    case CHARV(c: Char)  
    case SEQV(v1: VALUE, r2: VALUE )
    case LEFT(v: VALUE)
    case RIGHT(v: VALUE)
    case STARV(vs: List[VALUE])
    case ERRORVALUE(msg:String)
}
import VALUE._

abstract class Bit
case object Z extends Bit {
  override def toString = "0"
}
case object S extends Bit {
  override def toString = "1"
}
case object C extends Bit {
  override def toString = "7"
}
case object E extends Bit {
  override def toString = "8"
}
case object SE1 extends Bit {
  override def toString = "2"
}
case object SE2 extends Bit {
  override def toString = "3"
}
case object ST1 extends Bit {
  override def toString = "4"
}
case object ST2 extends Bit {
  override def toString = "5"
}

type Bits = List[Bit]

/* implicit val bitOrdering: Ordering[Bit] = Ordering.by {
  case C   => 0  // high priority for matching char?
  case E   => 1
  case Z   => 2
  case S   => 3
  case SE1 => 3
  case SE2 => 3
} */

val bitWeight: Bit => Double = {
  case C   => 100.0
  case Z   => 0
  case S   => -0.25
  case SE1 => -0.25
  case SE2 => -0.25
  case ST1 => -0.25
  case ST2 => -0.25
  case E   => -2.0

}

def totalBitsWeight(bs: Bits): Double = {
  val n = bs.length
  if (n == 0) return 0.0
  val weightedSum = bs.zipWithIndex.map {
    case (bit, idx) => bitWeight(bit) / (idx + 1) 
  }.sum

  (weightedSum) * 100.0 // average reward per C, scaled to percentage
}



def bitsToInts(bs: Bits): List[Int] = bs.map {
  case Z    => 0
  case S    => 1
  case SE1  => 2
  case SE2  => 3
  case C    => 7
  case E    => 8
  case ST1  => 4
  case ST2  => 5
}

def convertMtoDBit2(bs: Bits): Bits = bs.flatMap {
    case ST1 => Some(Z)  // ST1 (4) => 0
    case ST2 => Some(S)  // ST2 (5) => 1
    case Z   => Some(Z)  //  0 => 0
    case S   => Some(S)  //  1 => 1
    case _   => None     // discard 
  }


// standard regexes
enum Rexp {
  case ZERO 
  case ONE 
  case CHAR(c: Char) 
  case ALT(r1: Rexp, r2: Rexp) 
  case SEQ(r1: Rexp, r2: Rexp) 
  case STAR(r: Rexp) 
  case NTIMES(r: Rexp, n: Int)
  case NOT(r: Rexp) 
  case POINT(bs: Bits, r: Rexp)
  case INIT(r: Rexp)
}

import Rexp._

def size(r: Rexp) : Int = (r: @unchecked) match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_) => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r) => 1 + size(r)
  case NTIMES(r,n) => 1 + size(r)
  case INIT(r) => 1 + size(r)
}

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
// pretty-printing Rexps

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

def pp(e: Rexp) : String = (e: @unchecked) match {
  case ZERO => "0\n"
  case ONE => "1\n"
  case CHAR(c) => s"$c\n"
  case POINT(bs, CHAR(c)) => s"•$c:${bs.mkString(",")}\n" 
  case ALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => "SEQ\n" ++ pps(r1, r2)
  case STAR(r) => "STAR\n" ++ pps(r)
}
def pps(es: Rexp*) = indent(es.map(pp))

// Decode function to reconstruct match structure
def decode(bs: List[Int], r: Rexp): (VALUE, List[Int]) = (r: @unchecked) match {
  case ONE => (EMPTY, bs)
  case CHAR(c) => (CHARV(c), bs)
  case ALT(r1, r2) => bs match {
    case 0 :: rest => val (v, rem) = decode(rest, r1); (LEFT(v), rem)
    case 1 :: rest => val (v, rem) = decode(rest, r2); (RIGHT(v), rem)
    case _ => (ERRORVALUE("ALT ERROR"), bs)
  }
  case SEQ(r1, r2) =>
    val (v1, bs1) = decode(bs, r1)
    val (v2, bs2) = decode(bs1, r2)
    (SEQV(v1, v2), bs2)

  case STAR(r) => bs match {
    case 1 :: rest => (STARV(List()), rest)
    case 0 :: rest => 
      val (v, bs1) = decode(rest, r)
      val (STARV(vs), bs2) = decode(bs1, STAR(r)): @unchecked
      (STARV(v :: vs), bs2)
    case _ => (ERRORVALUE("ALT ERROR"), bs)
  }
}



/* // nullable 
def nullable(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(d) =>  false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(r) => true
  case POINT(_, r) => nullable(r)
  case NTIMES(r: Rexp, n: Int) => 
    if (n == 0) true else nullable(r)
}

// fin function from the paper
// checks whether a mark is in "final" position
def fin(r: Rexp) : Boolean = (r: @unchecked) match {
  case ZERO => false
  case ONE => false
  case CHAR(_) => false
  case POINT(_, CHAR(_)) => true
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
  case NTIMES(r: Rexp, n: Int) => fin(r)
}

def mkeps(r: Rexp) : Bits = r match {
  case ZERO => Nil
  case ONE => Nil
  case CHAR(c) => Nil
  case POINT(bs, CHAR(_)) => bs
  case ALT(r1, r2) => 
    if (nullable(r1)) Z :: mkeps(r1) else S :: mkeps(r2)  
  case SEQ(r1, r2) => mkeps(r1) ++ mkeps(r2)
  case STAR(r) => mkeps(r) ++ List(S)
  case NTIMES(r: Rexp, n: Int) => Nil
}

def mkfin(r: Rexp) : Bits = r match {
  case POINT(bs, CHAR(_)) => bs
  case ALT(r1, r2) => if (fin(r1)) mkfin(r1) else mkfin(r2)  
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin(r1) ++ mkeps(r2)
  case SEQ(r1, r2) => mkfin(r2)
  case STAR(r) => mkfin(r) ++ List(S)
  case NTIMES(r: Rexp, n: Int) => Nil
  case ZERO => Nil
  case ONE => Nil
  case CHAR(c) => Nil
}

// shift function from the paper
def shift(m: Boolean, bs: Bits, r: Rexp, c: Char) : Rexp = (r: @unchecked) match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if (m && d == c) POINT(bs, CHAR(d)) else CHAR(d)
  case POINT(_, CHAR(d)) => if (m && d == c) POINT(bs, CHAR(d)) else CHAR(d)
  case ALT(r1, r2) => ALT(shift(m, bs :+ Z, r1, c), shift(m, bs :+ S, r2, c))
  case SEQ(r1, r2) if m && nullable(r1) => SEQ(shift(m, bs, r1, c), shift(true, bs ::: mkeps(r1), r2, c))
  case SEQ(r1, r2) if fin(r1) => SEQ(shift(m, bs, r1, c), shift(true, mkfin(r1), r2, c))
  case SEQ(r1, r2) => SEQ(shift(m, bs, r1, c), shift(false, Nil, r2, c))
  case STAR(r) if m && fin(r) => STAR(shift(true, bs ::: (mkfin(r) :+ Z), r, c))
  case STAR(r) if fin(r) => STAR(shift(true, mkfin(r) :+ Z, r, c)) 
  case STAR(r) if m => STAR(shift(m, bs, r, c))
  case STAR(r) => STAR(shift(false, Nil, r, c))
  case NTIMES(r, n) => 
    if (n == 0) r else {
      if (m || fin(r)) NTIMES(shift(m || fin(r), bs, r, c), n)
      else NTIMES(shift(false, Nil, r, c), n)       
    }
}

// the main matching function (by using BINIT only in 
// the first step a mark is shifted into the Rexp)
def mat(r: Rexp, s: List[Char]) : Rexp = s match {
  case Nil => r
  case c::cs => cs.foldLeft(shift(true, Nil, r, c))((r, c) => shift(false, Nil, r, c))
}

def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s))

def matcher2(r: Rexp, s: List[Char]) : Rexp =
  if (s == Nil)
     if(nullable(r)) r else ZERO 
  else mat(r, s)
  
def lex(r: Rexp, s: List[Char]) : Option[Bits] = {
  if matcher(r, s)
  then Some(if (s == Nil) mkeps(r) else mkfin(mat(r, s)))
  else None
}

// pretty-printing Rexps

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

def pp(e: Rexp) : String = (e: @unchecked) match {
  case ZERO => "0\n"
  case ONE => "1\n"
  case CHAR(c) => s"$c\n"
  case POINT(bs, CHAR(c)) => s"•$c:${bs.mkString(",")}\n" 
  case ALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => "SEQ\n" ++ pps(r1, r2)
  case STAR(r) => "STAR\n" ++ pps(r)
  case NTIMES(r, n) => 
    s"NTIMES{$n}\n" ++ pps(r)
}
def pps(es: Rexp*) = indent(es.map(pp))




@main
def test1() = {
  println("=====Test====")
  val rexp=SEQ(ALT(ONE,CHAR('c')) , ALT(SEQ(CHAR('c'),CHAR('c')), CHAR('c')) )
  val s = "cc".toList
  println("=string=")
  println(s)

  println(s"mkfin=${mkfin(matcher2(rexp,s))}")
  println(s"mkeps=${mkeps(matcher2(rexp,s))}")
}

@main
def test2() = {
  println("=====Test2====")
  //val rexp = ALT(("b"|"a") , SEQ("a","a"))
 //val rexp =  ("b"|"a") | (("a"|"b") ~ ("a"|"c"))
  //val rexp =  ("a"~"cc")| ("a"|"cc")

  val rexp = (("bc"|"a") ~ ( ("a"|"bc")) )
  val s = "bcbc".toList
  println("=string=")
  println(s)
  println(s"=shift ${s(0)}=")
  println(pp(mat(rexp, s.take(1))))

    println(s"=shift ${s(1)}=")
  println(pp(mat(rexp, s.take(2)))) 


  println(s"=shift ${s(2)}=")
  println(pp(mat(rexp, s.take(3)))) 

  println(s"=shift ${s(3)}=")
  println(pp(mat(rexp, s.take(4))))  
}

 */




