import $file.rexp, rexp._, rexp.Rexp._, rexp.VALUE._

abstract class ARexp
case class BDZERO() extends ARexp
case class BDONE(bs: List[Int]) extends ARexp
case class BDCHAR(bs: List[Int], c: Char) extends ARexp
case class BDALTs(bs: List[Int], r1: ARexp, r2: ARexp) extends ARexp
case class BDSEQ(bs: List[Int], r1: ARexp, r2: ARexp) extends ARexp
case class BDSTAR(bs: List[Int], r: ARexp) extends ARexp
case class BDNTIMES(bs: List[Int], r: ARexp, n: Int) extends ARexp

// Convert standard regex to bitcoded regex
def fuse(bs: List[Int], r: ARexp): ARexp = r match {
  case BDZERO() => BDZERO()
  case BDONE(bs2) => BDONE(bs ++ bs2)
  case BDCHAR(bs2, c) => BDCHAR(bs ++ bs2, c)
  case BDALTs(bs2, r1, r2) => BDALTs(bs ++ bs2, r1, r2)
  case BDSEQ(bs2, r1, r2) => BDSEQ(bs ++ bs2, r1, r2)
  case BDSTAR(bs2, r) => BDSTAR(bs ++ bs2, r)
  case BDNTIMES(bs2, r, n) => BDNTIMES(bs ++ bs2, r, n)
}

def internalize(r: Rexp): ARexp = (r: @unchecked) match {
  case ZERO => BDZERO()
  case ONE => BDONE(List())
  case CHAR(c) => BDCHAR(List(), c)
  case ALT(r1, r2) => BDALTs(List(), fuse(List(0), internalize(r1)), fuse(List(1), internalize(r2)))
  case SEQ(r1, r2) => BDSEQ(List(), internalize(r1), internalize(r2))
  case STAR(r) => BDSTAR(List(), internalize(r))
  case NTIMES(r, n) => BDNTIMES(List(), internalize(r), n)
  case NOT(r) => BDZERO()
}


// Nullable function
def bnullable(r: ARexp): Boolean = r match {
  case BDZERO() => false
  case BDONE(_) => true
  case BDCHAR(_, _) => false
  case BDALTs(_, r1, r2) => bnullable(r1) || bnullable(r2)
  case BDSEQ(_, r1, r2) => bnullable(r1) && bnullable(r2)
  case BDSTAR(_, _) => true
  case BDNTIMES(_, r, n) => if (n == 0) true else bnullable(r)
}

// Compute derivative with bitcodes
def bder(c: Char, r: ARexp): ARexp = r match {
  case BDZERO() => BDZERO()
  case BDONE(_) => BDZERO()
  case BDCHAR(bs, d) => if (c == d) BDONE(bs) else BDZERO()
  case BDALTs(bs, r1, r2) => BDALTs(bs, bder(c, r1), bder(c, r2))
  case BDSEQ(bs, r1, r2) => 
    if (bnullable(r1)) BDALTs(bs, BDSEQ(List(), bder(c, r1), r2), fuse(bmkeps(r1), bder(c, r2)))
    else BDSEQ(bs, bder(c, r1), r2)
  case BDSTAR(bs, r) => BDSEQ(bs :+ 0, bder(c, r), BDSTAR(List(), r))
  
  case BDNTIMES(bs, r, n) => 
    if (n == 0) BDZERO()
    else BDSEQ(bs :+ 0, bder(c, r), BDNTIMES(List(), r, n - 1))
}

// Compute derivative with respect to a string
def bders(s: List[Char], r: ARexp): ARexp = s match {
  case Nil => r
  case c :: cs => bders(cs, bder(c, r))
}

// Extract the bitcode
def bmkeps(r: ARexp): List[Int] = r match {
  case BDONE(bs) => bs
  case BDALTs(bs, r1, r2) => bs ++ (if (bnullable(r1)) bmkeps(r1) else bmkeps(r2))
  case BDSEQ(bs, r1, r2) => bs ++ bmkeps(r1) ++ bmkeps(r2)
  case BDSTAR(bs, _) => bs :+ 1
  case BDNTIMES(bs, r, n) => if (n == 0) bs :+ 1 else bs ++ List(0) ++ bmkeps(r) ++ bmkeps(BDNTIMES(List(), r, n - 1))
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