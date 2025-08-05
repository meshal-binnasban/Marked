//
// Algorithm from "A Play on Regular Expressions"
//
// augmented with bitsequences



import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate//, regenerate._
import $file.rebit

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

// nullable 
def nullable(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(d) =>  false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(r) => true
  case POINT(_, r) => nullable(r)
}
*/ 


// decoding of a value from a bitsequence
def decode_aux(r: Rexp, bs: Bits) : (Val, Bits) = (r, bs) match {
  case (ONE, bs) => (Empty, bs)
  case (CHAR(c), bs) => (Chr(c), bs)
  case (ALT(r1, r2), Lf::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    (Left(v), bs1)
  }
  case (ALT(r1, r2), Ri::bs) => {
    val (v, bs1) = decode_aux(r2, bs)
    (Right(v), bs1)
  }
  case (SEQ(r1, r2), bs) => {
    val (v1, bs1) = decode_aux(r1, bs)
    val (v2, bs2) = decode_aux(r2, bs1)
    (Sequ(v1, v2), bs2)
  }
  case (STAR(_), En::bs) => (Stars(Nil), bs)
  case (STAR(r1), Nx::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    val (Stars(vs), bs2) = (decode_aux(STAR(r1), bs1)  : @unchecked)
    (Stars(v::vs), bs2)
  }
  /*case (STAR(r1), bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    val (Stars(vs), bs2) = (decode_aux(STAR(r1), bs1)  : @unchecked)
    (Stars(v::vs), bs2)
  }*/
}

def dec2(r: Rexp, bs: Bits) = decode_aux(r, bs) match {
  case (v, Nil) => v
  case _ => throw new Exception("Not decodable")
}

def mkeps(r: Rexp) : Bits = r match {
  case ONE => Nil
  case POINT(bs, CHAR(_)) => bs
  case ALT(r1, r2) => 
    if (nullable(r1)) Lf :: mkeps(r1) else Ri :: mkeps(r2)  
  case SEQ(r1, r2) => mkeps(r1) ++ mkeps(r2)
  case STAR(r) => List(En)
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

def mkfin(r: Rexp) : Bits = r match {
  case POINT(bs, CHAR(_)) => bs
  case ALT(r1, r2) => if (fin(r1)) mkfin(r1) else mkfin(r2)  
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin(r1) ++ mkeps(r2)
  case SEQ(r1, r2) => mkfin(r2)
  case STAR(r) => mkfin(r) ++ List(En)
}

def mkfin2(r: Rexp) : Set[Bits] = r match {
  case POINT(bs, CHAR(_)) => Set(bs)
  case ALT(r1, r2) if fin(r1) && fin(r2) => mkfin2(r1) | mkfin2(r2)
  case ALT(r1, r2) if fin(r1) => mkfin2(r1)
  case ALT(r1, r2) if fin(r2) => mkfin2(r2)                     
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin2(r1).map(_ ++ mkeps(r2))
  case SEQ(r1, r2) => mkfin2(r2)
  case STAR(r) => mkfin2(r).map(_ ++ List(En))
}


// shift function from the paper
def shift(m: Boolean, bs: Bits, r: Rexp, c: Char) : Rexp = (r: @unchecked) match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if (m && d == c) POINT(bs, CHAR(d)) else CHAR(d)
  case POINT(_, CHAR(d)) => if (m && d == c) POINT(bs, CHAR(d)) else CHAR(d)

  case ALT(r1, r2) => ALT(shift(m, bs :+ Lf, r1, c), shift(m, bs :+ Ri, r2, c))
  
  case SEQ(r1, r2) if m && nullable(r1) => 
    SEQ(shift(m, bs, r1, c), shift(true, bs ::: mkeps(r1), r2, c))
  case SEQ(r1, r2) if fin(r1) => SEQ(shift(m, bs, r1, c), shift(true, mkfin(r1), r2, c))
  case SEQ(r1, r2) => SEQ(shift(m, bs, r1, c), shift(false, Nil, r2, c))
  
  case STAR(r) if m && fin(r) => STAR(shift(true, bs ::: (mkfin(r) :+ Nx), r, c))
  case STAR(r) if fin(r) => STAR(shift(true, mkfin(r) :+ Nx, r, c)) 
  case STAR(r) if m => STAR(shift(m, bs :+ Nx, r, c))
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

def lex(r: Rexp, s: List[Char]) : Option[Set[Bits]] = {
  if matcher(r, s)
  then Some(if (s == Nil) Set(mkeps(r)) else mkfin2(mat(r, s)))
  else None
}


def lexer(r: Rexp, s: List[Char]) : Option[Set[Val]] = {
  lex(r, s).map(_.map(dec2(r, _)))
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




@main
def test1() = {
  println("=====Test====")
  val br2 = (ONE | "c") ~ ("cc" | "c")
  val s = "cc".toList
  println("=string=")
  println(s)
  println(s"=shift ${s(0)}=")
  println(pp(mat(br2, s.take(1))))

  println(s"=shift ${s(1)}=")
  println(pp(mat(br2, s.take(2))))

  println(s"=final list=")
  println(lex(br2, s.take(2)))
}

@main
def test2() = {
  println("=====Test====")
  //val br2 = SEQ(CHAR('a'),ALT(ONE,ONE))
  //val br2 = SEQ(ALT(ONE,CHAR('c')),ALT(SEQ(CHAR('c'),CHAR('c')),CHAR('c')))
  val br2 = ALT(ONE, STAR("c" | "d"))
  val s = "ccc".toList
  println("=string=")
  println(s)
  println(s"=shift ${s(0)}=")
  println(pp(mat(br2, s.take(1))))

  println(s"=shift ${s(1)}=")
  println(pp(mat(br2, s.take(2))))

  
  println(s"=shift ${s(2)}=")
  println(pp(mat(br2, s.take(3))))

  //println(s"=shift ${s(3)}=")
  //println(pp(mat(br2, s.take(4))))
  
  println(s"=final list=")
  println(lex(br2, s.take(3)))
}

@main
def test3() = {
  println("=====Test====")
  val br2 = STAR("e" | ("d" | "c"))
  val s = "ccc".toList
  println("=string=")
  println(s)
  println(s"=shift ${s(0)}=")
  println(pp(mat(br2, s.take(1))))

  println(s"=shift ${s(1)}=")
  println(pp(mat(br2, s.take(2))))

  println(s"=shift ${s(2)}=")
  println(pp(mat(br2, s.take(3))))

  println(s"=final list=")
  println(lex(br2, s.take(3)))
  println(s"=reference list=") 
  println(rebit.lex(br2, s.take(3)))
}

@main
def test4() = {
  println("=====Test====")
  val br2 = STAR("c")
  val s = "c".toList
  println("=string=")
  println(s)
  println(s"=shift ${s(0)}=")
  println(pp(mat(br2, s.take(1))))

  println(s"=final list=")
  println(lex(br2, s.take(1)))
  println(s"=reference list=") 
  println(rebit.lex(br2, s.take(1)))
}

import scala.util._

@main
def test5() = {
  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE),
        (0, _ => ZERO),
        (0, _ => CHAR('a')),
        (0, _ => CHAR('b')),
        (0, _ => CHAR('c')),
        (1, cs => STAR(cs(0))),
        (2, cs => ALT(cs(0), cs(1))),
        (2, cs => SEQ(cs(0), cs(1)))
      )

  val alphabet = LazyList('a', 'b')
  for (i <- (0L to 10_000_000L)) {
    val r = enumerate.decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "")
      { val v1s = Try(lexer(r, s.toList)).getOrElse(None)
        val v2 = rebit.blexer(r, s)
        if (v1s.isDefined && !v1s.get.contains(v2)) {
          println(s"reg: $r str: $s")
          println(s"mark: ${v1s.get} bder: $v2")
          println(s"mark: ${lex(r, s.toList).get} bder: ${rebit.lex(r, s.toList)}")
          System.exit(1)
        }
      }
  }
}





