
package ammonite
package $file
import _root_.ammonite.interp.api.InterpBridge.{
  value => interp
}
import _root_.ammonite.interp.api.InterpBridge.value.{
  exit,
  scalaVersion
}
import _root_.ammonite.interp.api.IvyConstructor.{
  ArtifactIdExt,
  GroupIdExt
}
import _root_.ammonite.compiler.CompilerExtensions.{
  CompilerInterpAPIExtensions,
  CompilerReplAPIExtensions
}
import _root_.ammonite.runtime.tools.{
  browse,
  grep,
  time,
  tail
}
import _root_.ammonite.compiler.tools.{
  desugar,
  source
}
import _root_.mainargs.{
  arg,
  main
}
import _root_.ammonite.repl.tools.Util.{
  PathRead
}
import _root_.ammonite.repl.ReplBridge.value.{
  codeColorsImplicit
}


object play{
/*<script>*///
// Algorithm from "A Play on Regular Expressions"
//
//
// Call with
//
//   amm play.sc test1
//   amm play.sc test2
//   ...

// standard regexes
enum Rexp {
  case ZERO 
  case ONE 
  case CHAR(c: Char)
  case ALT(r1: Rexp, r2: Rexp) 
  case SEQ(r1: Rexp, r2: Rexp) 
  case STAR(r: Rexp) 
  case NT(r: Rexp, n: Int)
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


// marked regular expressions
enum REG {
  case BZERO
  case BONE
  case BCHAR(b: Boolean, c: Char)
  case BALT(r1: REG, r2: REG)
  case BSEQ(r1: REG, r2: REG)
  case BSTAR(r: REG)
  case BNT(r: REG, n: Int)
}

import REG._

// internalises a regular expression into a marked
// regular expression
def intern(r: Rexp) : REG = r match {
  case ZERO => BZERO
  case ONE => BONE
  case CHAR(c) => BCHAR(false, c)
  case ALT(r1, r2) => BALT(intern(r1), intern(r2))
  case SEQ(r1, r2) => BSEQ(intern(r1), intern(r2))
  case STAR(r) => BSTAR(intern(r))
  case NT(r, n) => BNT(intern(r), n)
}


// flatten function (removes any marking)
// translates a marked regex back into a standard regex
def fl(r: REG) : Rexp = r match {
  case BZERO => ZERO
  case BONE => ONE
  case BCHAR(_, d) =>  CHAR(d)
  case BALT(r1, r2) => ALT(fl(r1), fl(r2))
  case BSEQ(r1, r2) => SEQ(fl(r1), fl(r2))
  case BSTAR(r) => STAR(fl(r))
  case BNT(r, n) => NT(fl(r), n)
}

// nullable for marked regexes
def nullable(r: REG) : Boolean = r match {
  case BZERO => false
  case BONE => true
  case BCHAR(b, d) =>  false
  case BALT(r1, r2) => nullable(r1) || nullable(r2)
  case BSEQ(r1, r2) => nullable(r1) && nullable(r2)
  case BSTAR(r) => true
  case BNT(r, n) => if (n == 0) true else nullable(r)
}

// fin function from the paper
// checks whether a mark is in "final" position
def fin(r: REG) : Boolean = r match {
  case BZERO => false
  case BONE => false
  case BCHAR(b, _) => b
  case BALT(r1, r2) => fin(r1) || fin(r2)
  case BSEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case BSTAR(r) => fin(r)
  case BNT(r, n) => if (n == 0 || nullable(r)) fin(r) else false
}//else if nullable(r) then fin(r) 

// shift function from the paper
def shift(m: Boolean, r: REG, c: Char) : REG = {
  //println(s"mode: $m")
  r match {
  case BZERO => BZERO
  case BONE => BONE
  case BCHAR(_ , d) => BCHAR(m && d == c, d)
  case BALT(r1, r2) => BALT(shift(m, r1, c), shift(m, r2, c)) 
  case BSEQ(r1, r2) =>
    //BSEQ(shift(m, r1, c), shift((m && nullable(r1)) || fin(r1), r2, c))
    //println(s"cond: ${m && nullable(r1)}")
    if (m && nullable(r1))
    then {
      //println("if case")
      BALT(BSEQ(shift(m, r1, c), shift(fin(r1), r2, c)), shift(true, r2, c))
    }
    else {
      //println("else case")
      BSEQ(shift(m, r1, c), shift(fin(r1), r2, c))   
    }  
  case BSTAR(r) => BSTAR(shift(m || fin(r), r, c))
  case BNT(r, n) => if (n == 0) BNT(r, n) else 
                    if (m || fin(r)) BNT(shift(m || fin(r), r, c), n - 1)
                    else BNT(shift(false, r, c), n)
}
}

// the main matching function (by using BINIT only in 
// the first step a mark is shifted into the REG)
def mat(r: REG, s: List[Char]) : REG = s match {
  case Nil => r
  case c::cs => cs.foldLeft(shift(true, r, c))((r, c) => shift(false, r, c))
}

def matcher(r: REG, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s))



// pretty-printing REGs

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

def pp(e: REG) : String = e match {
  case BZERO => "0\n"
  case BONE => "1\n"
  case BCHAR(b, c) => if (b) s"•$c\n" else s"$c\n"
  case BALT(r1, r2) => s"BALT{fin=${fin(r1)}, null=${nullable(r1)}}\n" ++ pps(r1, r2)
  case BSEQ(r1, r2) => s"BSEQ{fin=${fin(r1)}, null=${nullable(r1)}}\n" ++ pps(r1, r2)
  case BSTAR(r) => "BSTAR\n" ++ pps(r)
  case BNT(r, n) => s"BNT{$n}{fin=${fin(r)}, null=${nullable(r)}}\n" ++ pps(r)
}
def pps(es: REG*) = indent(es.map(pp))

@main
def test1() = {
  println("=====Test====")
  val br1 = SEQ(ALT("a", "ab"), ALT("bc", "c"))
  val br2 = intern(br1)
  val s = "abcd".toList
  println("=intern=")
  println(pp(br2) ++ "\n")
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
}

@main
def test2() = {
  println("=====Test====")
  val rexp = SEQ(ALT("a", "ab"), ALT("bc", ZERO))
  val breg = intern(rexp)
  val str1 = "abc".toList
  val str2 = "abcd".toList
  println(matcher(breg, str1))
  println(matcher(breg, str2))
  println(pp(breg))
}

@main
def test3() = {
  println("=====Test====")
  val rexp = ("a" ~ "b") ~ "c"
  val breg = intern(rexp)
  val str1 = "abc".toList
  println(s"start: $rexp")
  println("=============")
  for (n <- (1 to str1.length)) {
    val sl = str1.slice(0, n)
    println(s"shift: ${sl.last}")
    println(pp(mat(breg, sl)))
  }
}

@main
def test4() = {
  println("=====Test====")
  
  val rexp = (NT("a" | ONE, 2)) ~ (NT(CHAR('a'), 2))
  val breg = intern(rexp)

  val s = "aabb".toList

  println(s"start: $rexp")
  println("=============")
  println("=intern=")
  println(pp(breg) ++ "\n")
  println("=string=")
  println(s)
  println(s"=shift ${s(0)}=")
  println(pp(mat(breg, s.take(1))))

  println(s"=shift ${s(1)}=")
  println(pp(mat(breg, s.take(2))))

  println(s"=shift ${s(2)}=")
  println(pp(mat(breg, s.take(3))))

  println(s"=shift ${s(3)}=")
  println(pp(mat(breg, s.take(4))))

  println(matcher(breg, s))

  println(s"\n\n  ============================ \n\n")

  val reg=NT("a" | ONE , 2)
  val regi=intern(reg)

  val ss="aab".toList
  println(matcher(regi,ss))

  for (i <- ss.indices) {
  println(s"${i + 1}- =shift ${ss(i)}=")
  val sPart = ss.take(i + 1)
  println(pp(mat(regi, sPart)))
  }
}








/*</script>*/ /*<generated>*/
def $main() = { _root_.scala.Iterator[String]() }
  override def toString = "play"
  /*</generated>*/
}
