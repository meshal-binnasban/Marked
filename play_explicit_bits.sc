import scala.language.implicitConversions
import $file.rexp, rexp._, rexp.Rexp._, rexp.VALUE._
import $file.derivativesBitcode, derivativesBitcode._

// nullable 
def nullable(r: Rexp) : Boolean = (r: @unchecked) match {
  case ZERO => false
  case ONE => true
  case CHAR(d) =>  false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(r) => true
  case POINT(_, r) => nullable(r)
}

def mkeps(r: Rexp) : Bits = (r: @unchecked) match {
  case ONE => Nil
  case POINT(bs, CHAR(_)) => bs
  case ALT(r1, r2) => 
    if (nullable(r1)) Z :: mkeps(r1) else S :: mkeps(r2)  
  case SEQ(r1, r2) => mkeps(r1) ++ mkeps(r2)
  case STAR(r) => mkeps(r) ++ List(S)
  case CHAR(_) => Nil //for testing mkeps outside lex
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
}

//maybe add count matches?
def newMkfin(r: Rexp) : Bits = (r: @unchecked) match {
  case POINT(bs, CHAR(_)) => bs
  case ALT(r1, r2) => 
    if(fin(r1) && fin(r2)){
      val mkfin1=mkfin(r1)
      val mkfin2=mkfin(r1)
      println(s"mkfin(r1)=${mkfin(r1)}, mkfin(r2)=${mkfin(r2)}")
      //println(s"comparison > = ${mkfin1 > mkfin2}")
    }

    if (fin(r1)) mkfin(r1) else mkfin(r2)  
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin(r1) ++ mkeps(r2)
  case SEQ(r1, r2) => mkfin(r2)
  case STAR(r) => mkfin(r) ++ List(S)
}
def isGreater(a: List[Int], b: List[Int]): Boolean = {
  val len = a.length max b.length
  val aPad = List.fill(len - a.size)(0) ++ a
  val bPad = List.fill(len - b.size)(0) ++ b

  (aPad, bPad).zipped.exists(_ > _)
}


def mkfin(r: Rexp) : Bits = (r: @unchecked) match {
  case POINT(bs, CHAR(_)) => bs
  case ALT(r1, r2) =>
    if(fin(r1) && fin(r2)){
      val mkfin1=bitsToInts(mkfin(r1))
      val mkfin2=bitsToInts(mkfin(r2))
      println(s"mkfin(r1)=${mkfin1}, mkfin(r2)=${mkfin2}")
      println(s"comparison > = ${isGreater(mkfin1,mkfin2)}")
      if(isGreater(mkfin1,mkfin2)){
        mkfin(r1)
      }else{
        mkfin(r2)
      }
    }
    else
    if (fin(r1)) mkfin(r1) else mkfin(r2)  
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin(r1) ++ mkeps(r2)
  case SEQ(r1, r2) => mkfin(r2)
  case STAR(r) => mkfin(r) ++ List(S)
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
  if (s == Nil) r else mat(r, s)

def lex(r: Rexp, s: List[Char]) : Option[Bits] = {
  if matcher(r, s)
  then Some(if (s == Nil) mkeps(r) else mkfin(mat(r, s)))
  else None
}


// testing one/emptystring regex 
@main
def test1() = {
  println("=====Test With ONE====")
  val rexp=SEQ(ALT(ONE,CHAR('c')) , ALT(SEQ(CHAR('c'),CHAR('c')), CHAR('c')) )
  val s = "cc".toList

  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(rexp, sPart)))
  } 

  val finReg=matcher2(rexp,s)
  val bits=bitsToInts(lex(rexp, s).getOrElse(Nil))
  println(s"=final list=\n ${bits} ")

  println(s"${finReg}")
  println(s"\nmkeps: ${mkeps(finReg)}")
  println(s"mkfin: ${mkfin(finReg)}")
  println(s"\nDecoded value for Marked=${decode( bits, rexp)._1}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  println(s"\nDerivatives bitcode: $derivBitcode")
  println(s"\nDecoded value for derivatives=${decode( derivBitcode, rexp)._1}")
  
}

//testing seq,alt,char only regex
@main
def test2() = {
  println("=====Test With SEQ/ALT/CHAR only====")
  val rexp=SEQ(
    ALT(ALT(CHAR('a'),CHAR('b')),SEQ(CHAR('a'),CHAR('b'))) , 
    ALT( SEQ(CHAR('b'),CHAR('c')), ALT(CHAR('c'),CHAR('b'))) )
  val s = "abc".toList
  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(rexp, sPart)))
  } 

  val finReg=matcher2(rexp,s)
  val bits=bitsToInts(lex(rexp, s).getOrElse(Nil))
  println(s"=final list=\n ${bits} ")

  println(s"${finReg}")
  println(s"\nmkeps: ${mkeps(finReg)}")
  println(s"mkfin: ${mkfin(finReg)}")
  println(s"\nDecoded value for Marked=${decode( bits, rexp)._1}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  println(s"\nDerivatives bitcode: $derivBitcode")
  println(s"\nDecoded value for derivatives=${decode( derivBitcode, rexp)._1}")
}

@main
def test3() = {
  println("=====Test With SEQ/ALT/CHAR only====")
  val rexp=("a" | "ab") ~ ("b" | ONE)
  val s = "ab".toList
  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(rexp, sPart)))
  } 
  val finReg=matcher2(rexp,s)
  val bits=bitsToInts(lex(rexp, s).getOrElse(Nil))
  println(s"=final list=\n ${bits} ")

  println(s"${finReg}")
  println(s"\nmkeps: ${mkeps(finReg)}")
  println(s"mkfin: ${mkfin(finReg)}")
  println(s"Decoded value for Marked=${decode( bits, rexp)._1}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  println(s"derivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${decode( derivBitcode, rexp)._1}")
  
}





/* 
@main
def test3() = {
  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE),
        (0, _ => ZERO),
        (0, _ => CHAR('a')),
        (0, _ => CHAR('b')),
        (0, _ => CHAR('c')),
        //(1, cs => STAR(cs(0))),
        (2, cs => ALT(cs(0), cs(1))),
        (2, cs => SEQ(cs(0), cs(1)))
      )

  val alphabet = LazyList('a', 'b')
  for (i <- (0L to 100_000_000L)) {
    val r = enumerate.decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "")
      { val v1 = lex(r, s.toList)
        val v2 = rebit.lex(r, s.toList)
        if (v1.isDefined && v1.get != v2) {
          println(s"reg: $r str: $s")
          println(s"mark: ${v1.get} bder: $v2")
        }
      }
  }
} */




/* 
abstract class Bit
case object Z extends Bit {
  override def toString = "0"
}
case object S extends Bit {
  override def toString = "1"
}


type Bits = List[Bit]


// standard regexes
enum Rexp {
  case ZERO 
  case ONE 
  case CHAR(c: Char) 
  case ALT(r1: Rexp, r2: Rexp) 
  case SEQ(r1: Rexp, r2: Rexp) 
  case STAR(r: Rexp) 
  case POINT(bs: Bits, r: Rexp)
}

import Rexp._
 */

/* // some syntax sugar for regexes
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
 */