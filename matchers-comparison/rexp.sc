//
// Regular expressions and values
//
// can be tested with 
// 
//   amm rexp.sc

import scala.language.implicitConversions

abstract class Bit
/*
case object Z extends Bit {
  override def toString = "0"
}
case object S extends Bit {
  override def toString = "1"
}
*/

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

