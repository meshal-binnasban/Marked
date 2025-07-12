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
  mark: Boolean,
  bits: List[Bits],
  str: List[Char],
  consumed: List[Char],
  originalLength: Int=0
) {
  override def toString: String = {
    val bitsStr = bits.map(_.mkString(",")).mkString("/")
    s"(mark=$mark, bits=$bitsStr, remaining='${str.mkString}', consumed= ${consumed.mkString} , originalLength=$originalLength )"
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
// only used by re-generate to generate non-matching strings
case class NOT(r: Rexp) extends Rexp 
case class POINT(mk: Mark, r: Rexp) extends Rexp
case class APPOINT(mk: Mark, r: Rexp) extends Rexp // active/passive point

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
  case POINT(_, r) => nullable(r)
  
}

def der(c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) =>
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r) => SEQ(der(c, r), STAR(r))
  case NTIMES(r, n) => if (n == 0) ZERO else SEQ(der(c, r), NTIMES(r, n-1)) // new to testX1.
}

// the derivative w.r.t. a string (iterates der and simp)
def ders(s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, der(c, r))
}

// values
abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(v1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val
case class Nt(vs: List[Val], n: Int) extends Val // new to testX1.
//case object XXX extends Val



def mkeps(r: Rexp) : Val = r match {
  case ONE => Empty
  case ALT(r1, r2) =>
    if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
  case SEQ(r1, r2) => Sequ(mkeps(r1), mkeps(r2))
  case STAR(r) => Stars(Nil)
  case NTIMES(r, n) => Nt(Nil, 0)
}

def inj(r: Rexp, c: Char, v: Val) : Val = (r, v) match {
  case (STAR(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
  case (SEQ(r1, r2), Sequ(v1, v2)) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Left(Sequ(v1, v2))) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Right(v2)) => Sequ(mkeps(r1), inj(r2, c, v2))
  case (ALT(r1, r2), Left(v1)) => Left(inj(r1, c, v1))
  case (ALT(r1, r2), Right(v2)) => Right(inj(r2, c, v2))
  case (CHAR(d), Empty) => Chr(c)
}

// lexing functions without simplification
/* def lex(r: Rexp, s: List[Char]) : Val = s match {
  case Nil => if (nullable(r)) mkeps(r) else
    { throw new Exception("lexing error") }
  case c::cs => inj(r, c, lex(der(c, r), cs))
} */

def lex(r: Rexp, s: List[Char]): Val = s match
  case Nil =>
    if nullable(r) then
      val rv = mkeps(r)
      println(s"mkeps → $rv")
      rv
    else throw new Exception("lexing error")

  case c :: cs =>
    val derivative = der(c, r)
    println(draw(derivative))
    val pValue = lex(derivative, cs)
    val result = inj(r, c, pValue)
    println(s"inj $c → $result")
    result


// simplification
def simp(r: Rexp) : Rexp = r match {
  case ALT(r1, r2) => (simp(r1), simp(r2)) match {
    case (ZERO, r2s) => r2s
    case (r1s, ZERO) => r1s
    case (r1s, r2s) => //ALT (r1s, r2s)
      if (r1s == r2s) r1s else ALT (r1s, r2s)
  }
  case SEQ(r1, r2) =>  (simp(r1), simp(r2)) match {
    case (ZERO, _) => ZERO
    case (_, ZERO) => ZERO
    case (ONE, r2s) => r2s
    case (r1s, ONE) => r1s
    case (r1s, r2s) => SEQ(r1s, r2s)
  }
  case r => r
}


def draw_rs(rs: List[Rexp], prefix: String) : String = {
  val rsi = rs.iterator
  rsi.map(r => draw_r(r, prefix, rsi.hasNext)).mkString
}

def draw_r(e: Rexp, prefix: String, more: Boolean) : String = {
  val full_prefix = s"$prefix${if more then "├" else "└"}"
  val childPrefix = s"$prefix${if more then "│" else " "} "
  s"\n${full_prefix}" ++
  (e match {
    case ZERO => s"0"
    case ONE => s"1"
    case CHAR(c) => s"$c"
    case ALT(r1, r2) => s"ALT" ++ draw_rs(List(r1, r2), childPrefix)
    case SEQ(r1, r2) => s"SEQ" ++ draw_rs(List(r1, r2), childPrefix)
    case STAR(r) => s"STAR" ++ draw_r(r, childPrefix, false)
    case NTIMES(r, n) => s"NTIMES($n)" ++ draw_r(r, childPrefix, false) // new to testX1.
  })
}

def draw(e: Rexp) = 
    draw_r(e, "", false)


@main 
def test() = {
      val r = ("a" | "ab") ~ ("c" | "bc")
      println(s"reg: $r")
      //println(draw(r))
      println(s"Final Value= ${lex(r,"abc".toList)}")

}
