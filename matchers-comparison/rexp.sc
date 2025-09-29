//
// Regular expressions and values
//
// can be tested with 
// 
//   amm rexp.sc

import scala.language.implicitConversions

abstract class Bit
case object NxT extends Bit {
  override def toString = "Nx"
}
case object EnT extends Bit {
  override def toString = "En"
}
case object Ep extends Bit {
  override def toString = "Ep"
}
//original Bits
case object Lf extends Bit {
  override def toString = "L"
}
case object Ri extends Bit {
  override def toString = "R"
}
case object Nx extends Bit {
  override def toString = "N"
}
case object En extends Bit {
  override def toString = "E"
}

case object Z extends Bit {
  override def toString = "0"
}
case object S extends Bit {
  override def toString = "1"
}

type Bits = List[Bit]

case class Mark(
  //mark: Boolean,
  bits: Bits, //List[Bits],
  str: List[Char]
  //consumed: List[Char],
  //originalLength: Int=0
) {
  override def toString: String = {
    s"(str='${str.mkString}', bits=${bits.mkString(",")})"
    //s"(mark=$mark, bits=$bitsStr, remaining='${str.mkString}', consumed= ${consumed.mkString} , originalLength=$originalLength )"
  }
}

// regular expressions
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 
case class NTIMES(r: Rexp,n:Int) extends Rexp // new to testX1.
case class POINT(r: Rexp) extends Rexp
case class NOT(r: Rexp) extends Rexp 

abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(v1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val
case class Nt(vs: List[Val], n: Int) extends Val // new to testX1.
//case object XXX extends Val

//List[Bits] for file:play_explicit_List.sc

def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

// strings are coerced into Rexps
given Conversion[String, Rexp] = (s => charlist2rexp(s.toList))

//val HELLO : Rexp = "hello"

extension (r: Rexp) {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

def nullable(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, n) => n == 0 || nullable(r) 
  case POINT(r) => nullable(r)
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
  case ONE => s"1 \n"
  case CHAR(c) => s"$c\n"
  case ALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => "SEQ\n" ++ pps(r1, r2)
  case STAR(r) => s"STAR\n" ++ pps(r)
  case NTIMES(r, n) => s"NTIMES($n)\n" ++ pps(r)
}

def pps(es: Rexp*) = indent(es.map(pp))

// decoding of a value from a bitsequence
def decode_aux(r: Rexp, bs: Bits) : (Val, Bits) = ((r, bs): @unchecked) match {
  //case (_, I::bs) => decode_aux(r, bs)
  //case (_, I::b1::b2::bs) => decode_aux(r, b2::b1::bs)
  case (ONE, bs) => (Empty, bs)
  case (CHAR(c), bs) => (Chr(c), bs)
  case (ALT(r1, r2), Z::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    (Left(v), bs1)
  }
  case (ALT(r1, r2), S::bs) => {
    val (v, bs1) = decode_aux(r2, bs)
    (Right(v), bs1)
  }
  case (SEQ(r1, r2), bs) => {
    val (v1, bs1) = decode_aux(r1, bs)
    val (v2, bs2) = decode_aux(r2, bs1)
    (Sequ(v1, v2), bs2)
  }
  case (STAR(r1), Z::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    val (Stars(vs), bs2) = (decode_aux(STAR(r1), bs1)  : @unchecked)
    (Stars(v::vs), bs2)
  }
  case (STAR(r), S::bs) => (Stars(Nil), bs)

  case (NTIMES(r1,n), Z::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    val (Nt(vs,ns), bs2) = (decode_aux(NTIMES(r1,n-1), bs1)  : @unchecked)
    (Nt(v::vs,n), bs2)
  }
  case (NTIMES(_,_), S::bs) => (Nt(Nil,0), bs)
}

def decode(r: Rexp, bs: Bits) = decode_aux(r, bs) match {
  case (v, Nil) => v
  case _ => throw new Exception("Not decodable")
}

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}