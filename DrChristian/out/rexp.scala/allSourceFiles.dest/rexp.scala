//SOURCECODE_ORIGINAL_FILE_PATH=/Users/meshalbinnasban/Google Drive/KCL/Code Playground/Marked/DrChristian/rexp.scala
//SOURCECODE_ORIGINAL_CODE_START_MARKER
//| scalaVersion: 3.8.3
//| scalacOptions: ["-deprecation"]
//| mvnDeps: [org.scala-lang:scala3-library_3:3.8.3]

//
// Regular expressions and values
//
// can be tested with 
// 
//   amm rexp.sc

import scala.language.implicitConversions


enum Bit {
  case Z
  case S

  override def toString = this match {
    case Z => "0"
    case S => "1"
  }
}


type Bits = List[Bit]

enum Val {
  case Empty
  case Chr(c: Char)
  case Sequ(v1: Val, v2: Val)
  case Left(v: Val)
  case Right(v: Val)
  case Stars(vs: List[Val])
  //case NotV(v: Val)
  case NotV(r: Rexp)
  case Nt(vs: List[Val], n: Int)
}

import Val._

// regular expressions
enum Rexp {
  case ZERO
  case ONE
  case CHAR(c: Char)
  case ALT(r1: Rexp, r2: Rexp)
  case SEQ(r1: Rexp, r2: Rexp)
  case STAR(r: Rexp)
  case NTIMES(r: Rexp, n: Int)
  case OPTIONAL(r: Rexp) extends Rexp
  case AND(r1: Rexp, r2: Rexp) extends Rexp
  case NOT(r: Rexp)
}

import Rexp._



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
  case NTIMES(r, n) => if (n == 0) true else nullable(r)
  case NOT(r1) => !nullable(r1)
  case OPTIONAL(r) => true
}

def mkeps(r: Rexp) : Val = r match {
  case ONE => Empty
  case ALT(r1, r2) =>
    if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
  case SEQ(r1, r2) => Sequ(mkeps(r1), mkeps(r2))
  case STAR(r) => Stars(Nil)
  case NTIMES(r, n) => Nt(Nil, 0)
  case OPTIONAL(r) => Left(Empty)
  case NOT(r) => NotV(r)
  //case AND(r1,r2) => Sequ(mkeps(r1),mkeps(r2))
}

def mkepsBits(r: Rexp): Bits = r match {
  case ONE => Nil
  case ALT(r1, r2) => if (nullable(r1)) Bit.Z :: mkepsBits(r1) else Bit.S :: mkepsBits(r2)
  case SEQ(r1, r2) => mkepsBits(r1) ::: mkepsBits(r2)
  case STAR(r) => List(Bit.S)
  case NTIMES(r, n) =>
    if (n == 0) List(Bit.S)
    else Bit.Z :: (mkepsBits(r) ::: mkepsBits(NTIMES(r, n - 1)))
  case NOT(r) => Nil
  case OPTIONAL(r) => List(Bit.Z)
  case NOT(r) => Nil
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
  case NTIMES(r, n) => if (n == 0) ZERO else SEQ(der(c, r), NTIMES(r, n - 1))
  case OPTIONAL(r) => OPTIONAL(der(c, r))
}

// the derivative w.r.t. a string (iterates der and simp)
def ders(s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, der(c, r))
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
  case OPTIONAL(r) => s"OPTIONAL(${pp(r)})"
  case NOT(r) => s"NOT(${pp(r)})"
  case AND(r1, r2) => "AND\n" ++ pps(r1, r2)
 
}

def pps(es: Rexp*) = indent(es.map(pp))

//def main(args: Array[String]): Unit = ()
def decode_aux(r: Rexp, bs: Bits): (Val, Bits) = (r, bs) match {
  case (ONE, bs) => (Empty, bs)
  case (CHAR(c), bs) => (Chr(c), bs)
  case (ALT(r1, r2), Bit.Z :: bs) =>
    val (v, bs1) = decode_aux(r1, bs)
    (Left(v), bs1)
  case (ALT(r1, r2), Bit.S :: bs) =>
    val (v, bs1) = decode_aux(r2, bs)
    (Right(v), bs1)
  case (SEQ(r1, r2), bs) =>
    val (v1, bs1) = decode_aux(r1, bs)
    val (v2, bs2) = decode_aux(r2, bs1)
    (Sequ(v1, v2), bs2)
  case (STAR(r), Bit.Z :: bs) =>
    val (v, bs1) = decode_aux(r, bs)
    val (Stars(vs), bs2) = decode_aux(STAR(r), bs1)
    (Stars(v :: vs), bs2)
  case (STAR(_), Bit.S :: bs) => (Stars(Nil), bs)
  case (NTIMES(r, n), Bit.Z :: bs) =>
    val (v, bs1) = decode_aux(r, bs)
    val (Nt(vs, _), bs2) = decode_aux(NTIMES(r, n - 1), bs1)
    (Nt(v :: vs, n), bs2)
  case (NTIMES(r, n), Bit.S :: bs) => (Nt(Nil, n), bs)
  /* case (NOT(r), bs) =>
    val (v, bs1) = decode_aux(r, bs)
    (NotV(v), bs1) */
  case (NOT(r), bs) => (NotV(r), bs)
}

def decode(r: Rexp, bs: Bits): Val =
  decode_aux(r, bs) match {
    case (v, Nil) => v
    case _ => throw new Exception("Not decodable")
  }
type main = mainargs.main; private def rexp_scala_millScriptMainSelf = this; object MillScriptMain_rexp_scala { def main(args: Array[String]): Unit = this.getClass.getMethods.find(m => m.getName == "main" && m.getParameters.map(_.getType) == Seq(classOf[Array[String]]) && m.getReturnType == classOf[Unit]) match{ case Some(m) => m.invoke(rexp_scala_millScriptMainSelf, args); case None => mainargs.Parser(rexp_scala_millScriptMainSelf).runOrExit(args) }}