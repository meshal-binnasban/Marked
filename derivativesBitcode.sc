import $file.rexp, rexp._
import rexp.Rexp._
import scala.collection.immutable.List
/* 
enum Rexp {
  case ZERO 
  case ONE 
  case CHAR(c: Char) 
  case ALT(r1: Rexp, r2: Rexp) 
  case SEQ(r1: Rexp, r2: Rexp) 
  case STAR(r: Rexp)
  case NTIMES(r:Rexp,n:Int)
  case NOT(r: Rexp) 
}

import Rexp._ */

abstract class ARexp
case class BZERO() extends ARexp
case class BONE(bs: List[Int]) extends ARexp
case class BCHAR(bs: List[Int], c: Char) extends ARexp
case class BALTs(bs: List[Int], r1: ARexp, r2: ARexp) extends ARexp
case class BSEQ(bs: List[Int], r1: ARexp, r2: ARexp) extends ARexp
case class BSTAR(bs: List[Int], r: ARexp) extends ARexp
case class BNTIMES(bs: List[Int], r: ARexp, n: Int) extends ARexp


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


// Convert standard regex to bitcoded regex
def fuse(bs: List[Int], r: ARexp): ARexp = r match {
  case BZERO() => BZERO()
  case BONE(bs2) => BONE(bs ++ bs2)
  case BCHAR(bs2, c) => BCHAR(bs ++ bs2, c)
  case BALTs(bs2, r1, r2) => BALTs(bs ++ bs2, r1, r2)
  case BSEQ(bs2, r1, r2) => BSEQ(bs ++ bs2, r1, r2)
  case BSTAR(bs2, r) => BSTAR(bs ++ bs2, r)
  case BNTIMES(bs2, r, n) => BNTIMES(bs ++ bs2, r, n)
}


def internalize(r: Rexp): ARexp = r match {
  case ZERO => BZERO()
  case ONE => BONE(List())
  case CHAR(c) => BCHAR(List(), c)
  case ALT(r1, r2) => BALTs(List(), fuse(List(0), internalize(r1)), fuse(List(1), internalize(r2)))
  case SEQ(r1, r2) => BSEQ(List(), internalize(r1), internalize(r2))
  case STAR(r) => BSTAR(List(), internalize(r))
  case NTIMES(r, n) => BNTIMES(List(), internalize(r), n)
  case NOT(r) => BZERO()
}


// Nullable function
def bnullable(r: ARexp): Boolean = r match {
  case BZERO() => false
  case BONE(_) => true
  case BCHAR(_, _) => false
  case BALTs(_, r1, r2) => bnullable(r1) || bnullable(r2)
  case BSEQ(_, r1, r2) => bnullable(r1) && bnullable(r2)
  case BSTAR(_, _) => true
  case BNTIMES(_, r, n) => if (n == 0) true else bnullable(r)
}

// Compute derivative with bitcodes
def bder(c: Char, r: ARexp): ARexp = r match {
  case BZERO() => BZERO()
  case BONE(_) => BZERO()
  case BCHAR(bs, d) => if (c == d) BONE(bs) else BZERO()
  case BALTs(bs, r1, r2) => BALTs(bs, bder(c, r1), bder(c, r2))
  case BSEQ(bs, r1, r2) => 
    if (bnullable(r1)) BALTs(bs, BSEQ(List(), bder(c, r1), r2), fuse(bmkeps(r1), bder(c, r2)))
    else BSEQ(bs, bder(c, r1), r2)
  case BSTAR(bs, r) => BSEQ(bs :+ 0, bder(c, r), BSTAR(List(), r))
  case BNTIMES(bs, r, n) => 
    if (n == 0) BZERO()
    else BSEQ(bs :+ 0, bder(c, r), BNTIMES(List(), r, n - 1))
}

// Compute derivative with respect to a string
def bders(s: List[Char], r: ARexp): ARexp = s match {
  case Nil => r
  case c :: cs => bders(cs, bder(c, r))
}

// Extract the bitcode
def bmkeps(r: ARexp): List[Int] = r match {
  case BONE(bs) => bs
  case BALTs(bs, r1, r2) => bs ++ (if (bnullable(r1)) bmkeps(r1) else bmkeps(r2))
  case BSEQ(bs, r1, r2) => bs ++ bmkeps(r1) ++ bmkeps(r2)
  case BSTAR(bs, _) => bs :+ 1
  case BNTIMES(bs, r, n) => if (n == 0) bs :+ 1 else bs ++ List(0) ++ bmkeps(r) ++ bmkeps(BNTIMES(List(), r, n - 1))
}

enum VALUE {
    case EMPTY
    case CHARV(c: Char)  
    case SEQV(v1: VALUE, r2: VALUE )
    case LEFT(v: VALUE)
    case RIGHT(v: VALUE)
    case STARV(vs: List[VALUE])
    case ERRORVALUE
}
import VALUE._

// Decode function to reconstruct match structure
def decode(bs: List[Int], r: Rexp): (VALUE, List[Int]) = r match {
  case ONE => (EMPTY, bs)
  case CHAR(c) => (CHARV(c), bs)
  case ALT(r1, r2) => bs match {
    case 0 :: rest => val (v, rem) = decode(rest, r1); (LEFT(v), rem)
    case 1 :: rest => val (v, rem) = decode(rest, r2); (RIGHT(v), rem)
    case _ => (ERRORVALUE, bs)
  }
  case SEQ(r1, r2) =>
    val (v1, bs1) = decode(bs, r1)
    val (v2, bs2) = decode(bs1, r2)
    (SEQV(v1, v2), bs2)
  case STAR(r) => bs match {
    case 1 :: rest => (STARV(List()), rest)
    case 0 :: rest => 
      val (v, bs1) = decode(rest, r)
      val (STARV(vs), bs2) = decode(bs1, STAR(r))
      (STARV(v :: vs), bs2)
    case _ => (ERRORVALUE, bs)
  }
}

// Matcher function
def bmatcher(r: Rexp, s: String): Boolean = {
  val br = internalize(r)
  val finalR = bders(s.toList, br)
  bnullable(finalR)
}



@main
def test1() = {
// Run tests
//val regex1=ALT(ALT(CHAR('b'),CHAR('a')), SEQ(CHAR('a') ,CHAR('b') ))
//val regex3=STAR("a")
val regex=("a" | "ab") ~ ("c" | "bc")
val input = "abbc"
val br = internalize(regex)

val finalR = bders(input.toList, br)
val bitcode = bmkeps(finalR)
val decoded = decode(bitcode, regex)

println(s"Matched: ${bnullable(finalR)}")
println(s"Bitcode: $bitcode")
println(s"Decoded structure: ${decoded._1}")


}

