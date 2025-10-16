//
// Regular expressions and values
//
// can be tested with 
// 
//   amm rexp.sc

import scala.language.implicitConversions


abstract class Bit
case object Z extends Bit {
  override def toString = "0"
}
case object S extends Bit {
  override def toString = "1"
}

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

case class Mark( bits: Bits, str: List[Char]) {
  override def toString: String = {s"(str='${str.mkString}', bits=${bits.mkString(",")})"}
}

// regular expressions
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 
case class NTIMES(r: Rexp,n:Int) extends Rexp 
case class NOT(r: Rexp) extends Rexp // only used by re-generate to generate non-matching strings
case class POINT(bs: List[Bits], r: Rexp) extends Rexp

def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

// strings are coerced into Rexps
given Conversion[String, Rexp] = (s => charlist2rexp(s.toList))

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
//derivative lex function
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

// Derivative simplification
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

// decoding of a value from a bitsequence
def decode_aux(r: Rexp, bs: Bits) : (Val, Bits) = ((r, bs): @unchecked) match {
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
  
  case (NTIMES(r1,n), NxT::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    val (Nt(vs,ns), bs2) = (decode_aux(NTIMES(r1,n-1), bs1)  : @unchecked)
    (Nt(v::vs,n), bs2)
  }
  case (NTIMES(_,_), EnT::bs) => (Nt(Nil,0), bs)
}

def dec2(r: Rexp, bs: Bits) = decode_aux(r, bs) match {
  case (v, Nil) => v
  case _ => throw new Exception("Not decodable")
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
  case POINT(bs, CHAR(c)) => s"•$c {${bs.mkString(",")}}\n"
  case ALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => "SEQ\n" ++ pps(r1, r2)
  case STAR(r) => s"STAR\n" ++ pps(r)
  case NTIMES(r, n) => s"NTIMES($n)\n" ++ pps(r)
  
}

def pps(es: Rexp*) = indent(es.map(pp))

// extracts a string from a value
def flatten(v: Val) : String = v match {
   case Empty => ""
   case Chr(c) => c.toString
   case Left(v) => flatten(v)
   case Right(v) => flatten(v)
   case Sequ(v1, v2) => flatten(v1) ++ flatten(v2)
   case Stars(vs) => vs.map(flatten).mkString
   case Nt(vs, _) => vs.map(flatten).mkString
   //case Rec(_, v) => flatten(v)
 }

extension (ms: List[Mark]) {
  def <:+>(b: Bit): List[Mark] = 
    ms.map(m => m.copy(bits = m.bits :+ b))
  def <::+>(bs: Bits): List[Mark] = 
    ms.map(m=> m.copy(bits= m.bits ::: bs)) 
}
extension (m: Mark) {
  def <:+>(b: Bit): Mark = 
     m.copy(bits = m.bits :+ b)
  
}