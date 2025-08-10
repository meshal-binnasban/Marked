error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/rebit.sc:`<none>`.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/rebit.sc
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -rexp/r.
	 -rexp/r#
	 -rexp/r().
	 -r.
	 -r#
	 -r().
	 -scala/Predef.r.
	 -scala/Predef.r#
	 -scala/Predef.r().
offset: 5807
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/rebit.sc
text:
```scala
// Dearivative-based lexer using Brzozowski derivatives
//
//   tested with 
//
//     Ammonite Repl 2.5.3 (Scala 2.13.8 Java 17.0.1)
//     Scala 2.13.6 (OpenJDK 64-Bit Server VM, Java 17.0.1)
//
//   call with
//
//   amm re-bit.sc     or   scala re-bit.sc
//
//   

import scala.language.implicitConversions    
import scala.language.reflectiveCalls
import $file.rexp, rexp._

// annotated regular expressions
abstract class ARexp 
case object AZERO extends ARexp
case class AONE(bs: Bits) extends ARexp
case class ACHAR(bs: Bits, c: Char) extends ARexp
case class AALT(bs: Bits, r1: ARexp, r2: ARexp) extends ARexp 
case class ASEQ(bs: Bits, r1: ARexp, r2: ARexp) extends ARexp 
case class ASTAR(bs: Bits, r: ARexp) extends ARexp 
case class ANTIMES(bs: Bits, r: ARexp , n:Int) extends ARexp // new to testX1.

// Bitcoded + Annotation
//=======================

// fuse function
def fuse(bs: Bits, r: ARexp) : ARexp = r match {
  case AZERO => AZERO
  case AONE(cs) => AONE(bs ++ cs)
  case ACHAR(cs, c) => ACHAR(bs ++ cs, c)
  case AALT(cs, r1, r2) => AALT(bs ++ cs, r1, r2)
  case ASEQ(cs, r1, r2) => ASEQ(bs ++ cs, r1, r2)
  case ASTAR(cs, r) => ASTAR(bs ++ cs, r)
  case ANTIMES(cs, r,n) => ANTIMES(bs ++ cs, r,n)
}

def intern(r: Rexp) : ARexp = r match {
  case ZERO => AZERO
  case ONE => AONE(Nil)
  case CHAR(c) => ACHAR(Nil, c)
  case ALT(r1, r2) => 
    AALT(Nil, fuse(List(Lf), intern(r1)), fuse(List(Ri), intern(r2)))
  case SEQ(r1, r2) => ASEQ(Nil, intern(r1), intern(r2))
  case STAR(r) => ASTAR(Nil, intern(r))
  case NTIMES(r, n) => ANTIMES(Nil, intern(r),n)
  //case INIT(r1) => intern(r1)
}

// nullable function: tests whether the an (annotated) 
// regular expression can recognise the empty string
def bnullable (r: ARexp) : Boolean = r match {
  case AZERO => false
  case AONE(_) => true
  case ACHAR(_,_) => false
  case AALT(_, r1, r2) => bnullable(r1) || bnullable(r2)
  case ASEQ(_, r1, r2) => bnullable(r1) && bnullable(r2)
  case ASTAR(_, _) => true
  case ANTIMES(cs, r, n) => n == 0 || bnullable(r) // new to testX1.
}

def bmkeps(r: ARexp) : Bits = r match {
  case AONE(bs) => bs
  case AALT(bs, r1, r2) => 
    if (bnullable(r1)) bs ++ bmkeps(r1) else bs ++ bmkeps(r2)  
  case ASEQ(bs, r1, r2) => bs ++ bmkeps(r1) ++ bmkeps(r2)
  case ASTAR(bs, r) => bs ++ List(En)
  case ANTIMES(bs, r, n) => bs ++ List(EnT) // new to testX1.
}

// derivative of a regular expression w.r.t. a character
def bder(c: Char, r: ARexp) : ARexp = 
  r match {
  case AZERO => AZERO
  case AONE(_) => AZERO
  case ACHAR(bs, d) => if (c == d) AONE(bs) else AZERO
  case AALT(bs, r1, r2) => AALT(bs, bder(c, r1), bder(c, r2))
  case ASEQ(bs, r1, r2) => 
    if (bnullable(r1)) AALT(bs, ASEQ(Nil, bder(c, r1), r2), fuse(bmkeps(r1), bder(c, r2)))
    else ASEQ(bs, bder(c, r1), r2)
  case ASTAR(bs, r) => ASEQ(bs, fuse(List(Nx), bder(c, r)), ASTAR(Nil, r))
  case ANTIMES(bs, r, n) => 
    if (n == 0) AZERO else ASEQ(bs, fuse(List(NxT), bder(c, r)), ANTIMES(Nil, r, n-1)) // new to testX1.
}

// derivative w.r.t. a string (iterates bder)
def bders (r: ARexp, s: List[Char]) : ARexp = s match {
  case Nil => r
  case c::s => bders(bder(c, r), s)
}

def matcher (r: Rexp , s: String ) : Boolean =
  bnullable(bders(intern(r), s.toList))

// unsimplified lexing function (produces a value)
def blex(r: ARexp, s: List[Char]) : Bits = s match {
  case Nil => if (bnullable(r)) bmkeps(r) else throw new Exception("Not matched")
  case c::cs => blex(bder(c, r), cs)
}

def bsimp(r: ARexp): ARexp = r match
  case AALT(bs, r1, r2) => (bsimp(r1), bsimp(r2)) match 
      case (AZERO, r2s) => fuse(bs, r2s)
      case (r1s, AZERO) => fuse(bs, r1s)
      //case (AZERO, AZERO) => AZERO
      case (r1s, r2s) => AALT(bs, r1s, r2s)
  case ASEQ(bs, r1, r2) => (bsimp(r1), bsimp(r2)) match 
      case (AZERO, _) => AZERO
      case (_, AZERO) => AZERO
      case (r1s, r2s) => ASEQ(bs, r1s, r2s)
  case ASTAR(bs, r1) => bsimp(r1) match 
      case AZERO => AONE(bs :+ En)
      case r1s => ASTAR(bs, r1s)
  case ANTIMES(bs, r1, n) => bsimp(r1) match 
      case AZERO if n == 0 => AONE(bs :+ EnT)
      case AZERO => AZERO
      case r1s => ANTIMES(bs, r1s, n)
  case r => r

def unintern(r: ARexp): Rexp = r match {
  case AZERO => ZERO
  case AONE(_) => ONE                    
  case ACHAR(_, c) => CHAR(c)
  case AALT(_, r1, r2) => ALT(unintern(r1), unintern(r2))
  case ASEQ(_, r1, r2) => SEQ(unintern(r1), unintern(r2))
  case ASTAR(_, r) => STAR(unintern(r))
  case ANTIMES(_, r, n) => NTIMES(unintern(r), n)
}

def lex(r: Rexp, s: List[Char]) : Bits = blex(intern(r), s)

def blexer(r: Rexp, s: String) : Val = 
  decode(r, blex(intern(r), s.toList))


def pp(e: ARexp) : String = (e: @unchecked) match {
  case AZERO => "0\n"
  case AONE(bs) => s"1:${bs.mkString(",")}\n"
  case ACHAR(bs, c) => s"$c:${bs.mkString(",")}\n"
  case AALT(bs, r1, r2) => s"AALT:${bs.mkString(",")}\n" ++ pps(r1, r2)
  case ASEQ(bs, r1, r2) => s"ASEQ:${bs.mkString(",")}\n" ++ pps(r1, r2)
  case ASTAR(bs, r) => s"ASTAR:${bs.mkString(",")}\n" ++ pps(r)
  case ANTIMES(bs, r, n) => s"ANTIMES:${bs.mkString(",")}:$n\n" ++ pps(r) // new to testX1.
}
def pps(es: ARexp*) = indent(es.map(pp))

def ppr(e: Rexp) : String = (e: @unchecked) match {
  case ZERO => "0\n"
  case ONE => "1\n"
  case CHAR(c) => s"$c\n"
  case ALT(r1, r2) => "ALT\n" ++ pprs(r1, r2)
  case SEQ(r1, r2) => "SEQ\n" ++ pprs(r1, r2)
  case STAR(r) => "STAR\n" ++ pprs(r)
  case NTIMES(r, n) => s"NTIMES:$n\n" ++ pprs(r) // new to testX1.
}
def pprs(es: Rexp*) = indent(es.map(ppr))


@main
def test1() = {
  println("=====Test====")
  //val r = (ONE | "c") ~ ("cc" | "c")
  //val r = STAR("d" | "c")
  val r = ALT(ONE, STAR("a" | "b"))
  val br = intern(r)
  val s = "aaa".toList
  println("=string=")
  println(s)
  println("=orig=")
  println(ppr(@@r))
  println("=intern=")
  println(pp(br))
  println(s"=der ${s(0)}=")
  println(pp(bders(br, s.take(1))))

  println(s"=der ${s(1)}=")
  println(pp(bders(br, s.take(2))))

  println(s"=der ${s(2)}=")
  println(pp(bders(br, s.take(3))))

  println(s"=final result for ${s.take(3)}=")
  println(blex(br, s.take(3)))
}

@main
def test2() = {
  println("=====Test====")
  val r = ("a" | "ab") ~ ("b" | ONE)
  val br = intern(r)
  val s = "ab".toList
  println("=string=")
  println(s)
  println("=orig=")
  println(ppr(r))
  println("=intern=")
  println(pp(br))
  println(s"=der ${s(0)}=")
  println(pp(bders(br, s.take(1))))

  println(s"=der ${s(1)}=")
  println(pp(bders(br, s.take(2))))

  println(s"=final result for ${s.take(2)}=")
  println(blex(br, s.take(2)))
}
```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.