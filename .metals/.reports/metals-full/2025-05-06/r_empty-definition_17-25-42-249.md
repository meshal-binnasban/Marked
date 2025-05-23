error id: flatMap.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/play_explicit_bits.sc
empty definition using pc, found symbol in pc: flatMap.
empty definition using semanticdb

found definition using fallback; symbol flatMap
offset: 3765
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/play_explicit_bits.sc
text:
```scala
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

def mkeps(r: Rexp) : Set[Bits] = r match {
  case ONE => Set(Nil)
  case POINT(bs, CHAR(_)) => bs
  case ALT(r1, r2) => 
    if (nullable(r1)) mkeps(r1).map(Lf :: _) else mkeps(r2).map(Ri :: _)  
  case SEQ(r1, r2) => mkeps(r1) ++ mkeps(r2)
  case STAR(r) => Set(List(En))
}

def fin(r: Rexp) : Boolean = (r: @unchecked) match {
  case ZERO => false
  case ONE => false
  case CHAR(_) => false
  case POINT(_, CHAR(_)) => true
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
}

/* def mkfin(r: Rexp) : Bits = r match {
  case POINT(bs, CHAR(_)) => bs
  case ALT(r1, r2) => if (fin(r1)) mkfin(r1) else mkfin(r2) 

  case SEQ(r1, r2) if fin(r2) =>  mkfin(r2)
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin(r1) ++ mkeps(r2)

  case STAR(r) => mkfin(r) ++ List(En)
  //case POINT(bs, STAR(r)) => bs++mkfin(r)++ List(En)
  //case POINT(bs,STAR(r)) => bs
} */

def mkfin2(r: Rexp) : Set[Bits] = r match {
  case POINT(bs, CHAR(_)) => bs
  case ALT(r1, r2) if fin(r1) && fin(r2) => mkfin2(r1) | mkfin2(r2)
  case ALT(r1, r2) if fin(r1) => mkfin2(r1)
  case ALT(r1, r2) if fin(r2) => mkfin2(r2) 

  case SEQ(r1, r2) if fin(r1) && nullable(r2) =>mkfin2(r1).fl@@atMap(x => mkeps(r2).map(y => x ++ y)) | mkfin2(r2)// mkfin2(r1).map(_ ++ mkeps(r2)) 
  case SEQ(r1, r2) => mkfin2(r2)
  case STAR(r) => mkfin2(r).map(_ ++ List(En))
 // case POINT(bs, STAR(r)) => mkfin2(r).map(_ ++ List(En))
}

// shift function from the paper
def shift(m: Boolean, bs: Set[Bits], r: Rexp, c: Char) : Rexp = (r: @unchecked) match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if (m && d == c) POINT(bs, CHAR(d)) else CHAR(d)
  case POINT(_, CHAR(d)) => if (m && d == c) POINT(bs, CHAR(d)) else CHAR(d)
  case ALT(r1, r2) => ALT(shift(m, (bs).map(_ ++ List(Lf)), r1, c), shift(m, (bs).map(_ ++ List(Ri)), r2, c))

  case SEQ(r1, r2) if m && nullable(r1) => 
    SEQ(shift(m, bs, r1, c), shift(true, (bs+Nil).flatMap(b => (mkeps(r1)+Nil).map(f => b ++ f)) , r2, c))
  case SEQ(r1, r2) if fin(r1) => SEQ(shift(m, bs, r1, c), shift(true, mkfin2(r1), r2, c))
  case SEQ(r1, r2) => SEQ(shift(m, bs, r1, c), shift(false, Set(Nil), r2, c))
    
  // save old bits and shift empty list
  // if again, then add mkfin to old and shift empty again?
  case STAR(r) if m && fin(r) => STAR(shift(true, (bs).map(_ ++ List(Nx)) , r, c)) // bs:+Nx
  //case POINT(sbs, STAR(r)) if fin(r) => POINT(sbs,STAR(shift(true, (bs):+Nx, r, c)))

  case STAR(r) if fin(r) =>
    //STAR(shift(true, (for {b <- bs; f <- mkfin2(r)+Nil} yield b ++ f ++ List(Nx)), r, c))
    STAR(shift(true, (mkfin2(r)).map(_ ++ List(Nx)) , r, c)) //(mkfin(r)):+Nx (bs).flatMap(b => (mkfin2(r)+Nil).map(f => b ++ f ++ List(Nx)))
  case STAR(r) if m =>STAR(shift(m, (bs).map(_ ++ List(Nx)), r, c))
  case STAR(r) => STAR(shift(false, Set(Nil), r, c)) 
  
}

// testing nested star - X-2
def hasNestedMStar(r: Rexp): Boolean = {
  def containsMStar(r: Rexp): Boolean = r match {
    case STAR(_) => true
    case ALT(r1, r2) => containsMStar(r1) || containsMStar(r2)
    case SEQ(r1, r2) => containsMStar(r1) || containsMStar(r2)
    case _ => false
  }
  r match {
    case STAR(inner) => 
      if (containsMStar(inner)) true
      else hasNestedMStar(inner)
    case ALT(r1, r2) => hasNestedMStar(r1) || hasNestedMStar(r2)
    case SEQ(r1, r2) => hasNestedMStar(r1) || hasNestedMStar(r2)
    case _ => false
  }
}

// the main matching function (by using BINIT only in 
// the first step a mark is shifted into the Rexp)
def mat(r: Rexp, s: List[Char]) : Rexp = s match {
  case Nil => r
  case c::cs => cs.foldLeft(shift(true, Set(Nil), r, c))((r, c) => shift(false, Set(Nil), r, c))
}

def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s))

def lex(r: Rexp, s: List[Char]) : Option[Set[Bits]] = {
  if matcher(r, s)
  then Some(if (s == Nil) mkeps(r) else mkfin2(mat(r, s)))
  else None
}

def lexer(r: Rexp, s: List[Char]) : Option[Set[Val]] = {
  lex(r, s).map(_.map(dec2(r, _)))
}
  
/* def lex1(r: Rexp, s: List[Char]) : Option[Bits] = {
  if matcher(r, s)
  then Some(if (s == Nil) mkeps(r) else mkfin(mat(r, s)))
  else None
} */

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

//X-1 deleted - adjusted for test, X-1
def pp(e: Rexp) : String = (e: @unchecked) match {
  case ZERO => "0\n"
  case ONE => "1\n"
  case CHAR(c) => s"$c\n"
  case POINT(bs, CHAR(c)) => s"•$c:${bs.mkString(",")}\n" 
  //case POINT(bs, STAR(r))=> s"•STAR bs=${bs.mkString(",")}\n" ++ pps(r)
  case ALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => "SEQ\n" ++ pps(r1, r2)
  case STAR(r) => "STAR\n" ++ pps(r)
}
def pps(es: Rexp*) = indent(es.map(pp))


@main
def test1() = {
  println("=====Test====")
  val br2 = %("a")~(%("ab") ~ %("b"))
  val s = "ab".toList
  println("=string=")
  println(s)
  println(s"=shift ${s(0)}=")
  println(pp(mat(br2, s.take(1))))

  println(s"=shift ${s(1)}=")
  println(pp(mat(br2, s.take(2))))

  println(s"=final list=")
  println(lex(br2, s.take(2)))
  println(s"=reference list=") 
  println(rebit.lex(br2, s.take(2)))
}

//%("a") ~ ("aa"|"a"), works if bs:+Nx only.
@main
def test2() = {
  println("=====Test====")

  val br2= %("a") ~ ("aa"|"a")
  val s = "aaaaa".toList
  println("=string=")
  println(s)
  println(s"=shift ${s(0)}=")
  println(pp(mat(br2, s.take(1))))

  println(s"=shift ${s(1)}=")
  println(pp(mat(br2, s.take(2))))

  println(s"=shift ${s(2)}=")
  println(pp(mat(br2, s.take(3))))

  println(s"=shift ${s(3)}=")
  println(pp(mat(br2, s.take(4))))

  println(s"=shift ${s(4)}=")
  println(pp(mat(br2, s.take(5))))

  println(s"=final list=")
  println(lex(br2, s.take(5)))
  println(s"=reference list=") 
  println(rebit.lex(br2, s.take(5)))
}
//(ONE  |  %("c"|"d"))
@main
def test3() = {
  println("=====Test====")
  val br2 = (ONE  |  %("c"|"d"))
  //val br2= STAR( STAR("a") )
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
  println(s"=reference list=") 
  println(rebit.lex(br2, s.take(3)))
}
// (ONE|"a") ~ %("a")
@main
def test4() = {
  println("=====Test====")
  //val br2 =STAR( STAR("a") )
 // val br2=SEQ(STAR(CHAR('a')),STAR(CHAR('a')))
  //val br2=SEQ(ALT(ONE,CHAR('a')),STAR(CHAR('a')))
  val br2= (ONE|"a") ~ %("a")
  val s = "aaa".toList
  println(s"Regex:\n${pp(br2)}\n")
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

// ONE |  %( "a" | "aa" )
@main
def test5() = {
  println("=====Test====")
  val br2= ONE |  %( "a" | "aa" )
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}


import scala.util._

@main
def longTest1() = {
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

@main
def longTest2() = {
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
  var i=BigInt(0)
  val numRegexes=BigInt(10_000_000L)
  while(i<= numRegexes){
    val r = enumerate.decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "")
      { val v1s = Try(lexer(r, s.toList)).getOrElse(None)
        val v2 = rebit.blexer(r, s)
        if (v1s.isDefined && !v1s.get.contains(v2)) {
          
          if(!hasNestedMStar(r)){
          println(s"reg: $r str: $s")
          println(s"mark: ${v1s.get} bder: $v2")
          println(s"mark: ${lex(r, s.toList).get} bder: ${rebit.lex(r, s.toList)}")
          
          System.exit(1)
          }
        }
      }
      i+=1
  }//end whild
}






/* @main
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
  println(s"=reference list=") 
  println(rebit.lex(br2, s.take(2)))
} */
```


#### Short summary: 

empty definition using pc, found symbol in pc: flatMap.