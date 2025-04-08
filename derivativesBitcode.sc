import $file.rexp, rexp._, rexp.Rexp._, rexp.VALUE._

abstract class ARexp
case class BZERO() extends ARexp
case class BONE(bs: List[Int]) extends ARexp
case class BCHAR(bs: List[Int], c: Char) extends ARexp
case class BALTs(bs: List[Int], r1: ARexp, r2: ARexp) extends ARexp
case class BSEQ(bs: List[Int], r1: ARexp, r2: ARexp) extends ARexp
case class BSTAR(bs: List[Int], r: ARexp) extends ARexp
case class BNTIMES(bs: List[Int], r: ARexp, n: Int) extends ARexp

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

// Matcher function
def bmatcher(r: Rexp, s: String): Boolean = {
  val br = internalize(r)
  val finalR = bders(s.toList, br)
  bnullable(finalR)
}

@main
def test1() = {
// Run tests
val rexp=SEQ(ALT(ONE,CHAR('c')) , ALT(SEQ(CHAR('c'),CHAR('c')), CHAR('c')) )
val input = "cc"
val br = internalize(rexp)

val finalR = bders(input.toList, br)
val bitcode = bmkeps(finalR)
val decoded = decode(bitcode, rexp)

println(s"Regex: $rexp")
println(s"Matched: ${bnullable(finalR)}")
println(s"Bitcode: $bitcode")
println(s"Decoded structure: ${decoded._1}")
}

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