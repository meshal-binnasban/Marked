file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/.ammonite/scala-2.13.14/amm-3.0.0-2-6342755f/play_point3_BIT/src/ammonite/$file/play_point3_BIT.amm.sc.scala
### dotty.tools.dotc.core.TypeError$$anon$1: Toplevel definition EVIL2 is defined in
  <WORKSPACE>/play_point3_BIT.sc
and also in
  <WORKSPACE>/.ammonite/scala-2.13.14/amm-3.0.0-2-6342755f/play_point3_BIT/src/ammonite/$file/play_point3_BIT.amm.sc.scala
One of these files should be removed from the classpath.

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 10520
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/.ammonite/scala-2.13.14/amm-3.0.0-2-6342755f/play_point3_BIT/src/ammonite/$file/play_point3_BIT.amm.sc.scala
text:
```scala

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


object play_point3_BIT{
/*<start>*/
import scala.language.implicitConversions
import os.size

// Our regular expression type.
// NOTE: POINT is removed.
// CHAR now carries a Boolean flag (default false) that becomes true when it is "marked"
// during a match. BIT is now used only to record a bit decision at key match points.
enum Rexp {
  case ZERO 
  case ONE 
  case CHAR(c: Char, marked: Boolean = false)
  case ALT(r1: Rexp, r2: Rexp)
  case SEQ(r1: Rexp, r2: Rexp)
  case STAR(r: Rexp)
  case NTIMES(r: Rexp, n: Int, counter: Int = 0)
  // BIT nodes record branch/choice decisions (a list of bits).
  case BIT(r: Rexp, bs: List[Int])
}
import Rexp._

// Helper: when attaching bits, if the expression is already a BIT, flatten the bits.
def attachBits(r: Rexp, bs: List[Int]): Rexp = {
  if (bs.isEmpty) r
  else r match {
    case BIT(inner, innerBs) => BIT(inner, bs ++ innerBs)
    case _ => BIT(r, bs)
  }
}

// -----------------------
// Core Functions
// -----------------------

// nullable: true if the expression accepts the empty string.
def nullable(r: Rexp): Boolean = r match {
  case ZERO            => false
  case ONE             => true
  case CHAR(_, _)      => false
  case BIT(r, _)       => nullable(r)
  case ALT(r1, r2)     => nullable(r1) || nullable(r2)
  case SEQ(r1, r2)     => nullable(r1) && nullable(r2)
  case STAR(_)         => true
  case NTIMES(r, n, _) => if(n == 0) true else nullable(r)
}

// fin: true if the expression is in a final (matched) state.
// Now a CHAR returns true if its marked flag is true.
def fin(r: Rexp): Boolean = r match {
  case ZERO            => false
  case ONE             => false
  case CHAR(_, marked) => marked
  case BIT(r, _)       => fin(r)
  case ALT(r1, r2)     => fin(r1) || fin(r2)
  case SEQ(r1, r2)     => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r)         => fin(r)
  case NTIMES(r, n, counter) => counter == n && fin(r)
}

// -----------------------
// Shift Operation
// -----------------------
//
// The idea is that each shift call “consumes” a character.
// When a CHAR is shifted with m=true and its character equals c,
// we mark it (set the Boolean flag to true). Also, when a BIT node is
// encountered, we shift its inner expression and update its bit list.
// (This BIT is meant to record a branch decision; for example, in ALT,
// the left branch gets a 0 and the right a 1.)
def shift(m: Boolean, re: Rexp, c: Char): Rexp = re match {
  case ZERO => ZERO
  case ONE  => ONE
  case BIT(r, bs) =>
    // When shifting a BIT node, shift the inner expression and reattach its bits.
    attachBits(shift(m, r, c), bs)
  case CHAR(d, _) =>
    // If we are in "match mode" and the character equals c,
    // then mark it; otherwise leave unmarked.
    if (m && d == c) CHAR(d, true) else CHAR(d, false)
  case ALT(r1, r2) =>
    // In alternation, shift both branches with the same m.
    ALT(shift(m, r1, c), shift(m, r2, c))
  case SEQ(r1, r2) =>
    // In a sequence, the second part is shifted with m true if either
    // (a) we were matching and the first part is nullable or
    // (b) the first part is in a final state.
    SEQ(shift(m, r1, c), shift((m && nullable(r1)) || fin(r1), r2, c))
  case STAR(r) =>
    // For a star, we want to allow repeated matches.
    // We use m || fin(r) so that if a match was already made in r, then
    // further shifts are in matching mode.
    val shifted = STAR(shift(m || fin(r), r, c))
    // Here we attach a bit to record the star's "termination" decision.
    attachBits(shifted, mkeps(r) ++ List(0))
  case NTIMES(r, n, counter) =>
    if (counter == n) re
    else {
      if (m || fin(r))
        NTIMES(shift(m || fin(r), r, c), n, counter + 1)
      else
        NTIMES(shift(false, r, c), n, counter)
    }
}

// mat applies a sequence of shifts corresponding to the input string.
def mat(r: Rexp, s: List[Char]): Rexp = s match {
  case Nil    => r
  case c :: cs => cs.foldLeft(shift(true, r, c)) { (r, c) => shift(false, r, c) }
}

// matcher: returns true if s is accepted (using fin to decide finality).
def matcher(r: Rexp, s: List[Char]): Boolean =
  if (s.isEmpty) nullable(r) else fin(mat(r, s))

// matcher2: if s is empty, returns r if nullable, else ZERO; otherwise, returns the shifted regex.
def matcher2(r: Rexp, s: List[Char]): Rexp =
  if (s.isEmpty)
    if (nullable(r)) r else ZERO
  else
    mat(r, s)

// -----------------------
// VALUE and Decoding Functions
// -----------------------

enum VALUE {
  case ZEROV
  case ONEV
  case CHARV(c: Char)
  case UNMARKED(s: String)
  case SEQV(v1: VALUE, v2: VALUE)
  case LEFT(v: VALUE)
  case RIGHT(v: VALUE)
  case STARV(vs: List[VALUE])
}
import VALUE._

def decode(r: Rexp, bs: List[Int]): (VALUE, List[Int]) = r match {
  case BIT(r, bits) =>
    // Prepend this BIT node's bits and decode the inner expression.
    decode(r, bits ++ bs)
  case ONE =>
    (ONEV, bs)
  case CHAR(c, _) =>
    (CHARV(c), bs)
  case ALT(r1, r2) => bs match {
    case 0 :: bs1 =>
      val (v, bsp) = decode(r1, bs1)
      (LEFT(v), bsp)
    case 1 :: bs1 =>
      val (v, bsp) = decode(r2, bs1)
      (RIGHT(v), bsp)
    case _ =>
      (ZEROV, bs)
  }
  case SEQ(r1, r2) =>
    val (v1, bs2) = decode(r1, bs)
    val (v2, bs3) = decode(r2, bs2)
    (SEQV(v1, v2), bs3)
  case STAR(r) => bs match {
    case 1 :: bs1 =>
      (STARV(List()), bs1)
    case 0 :: bs1 =>
      val (v, bs2) = decode(r, bs1)
      val (STARV(vs), bsv) = decode(STAR(r), bs2)
      (STARV(v :: vs), bsv)
    case _ =>
      (STARV(List()), bs)
  }
  case NTIMES(r, n, counter) =>
    decode(r, bs)
  case ZERO =>
    (ZEROV, bs)
}

// mkeps: collects bits stored in BIT nodes and (for a marked CHAR) produces a bit.
def mkeps(r: Rexp): List[Int] = r match {
  case BIT(r, bs) =>
    // Collect BIT's bits plus whatever mkeps the inner expression produces.
    bs ++ mkeps(r)
  case ONE =>
    List()
  case CHAR(_, marked) =>
    if (marked) List(1) else List()
  case ALT(r1, r2) =>
    if (fin(r1)) mkeps(r1)
    else if (fin(r2)) mkeps(r2)
    else List()
  case SEQ(r1, r2) =>
    if (fin(r1) && nullable(r2)) mkeps(r1) ++ mkeps(r2)
    else if (fin(r2)) mkeps(r2)
    else List()
  case STAR(r) =>
    if (fin(r)) mkeps(r) ++ List(0) else List()
  case NTIMES(r, n, counter) =>
    if (counter == n && fin(r)) mkeps(r) else List()
  case ZERO =>
    List()
}

def fuse(cs: List[Int], r: Rexp): Rexp = r match {
  case BIT(inner, bs) => BIT(inner, cs ++ bs)
  case _              => BIT(r, cs)
}

// intern rewrites the expression so that branch decisions get an attached BIT.
// In particular, for an ALT we attach a 0 to the left branch and a 1 to the right.
def intern(r: Rexp): Rexp = r match {
  case ZERO                => ZERO
  case ONE                 => ONE
  case CHAR(c, _)          => CHAR(c, false)
  case ALT(r1, r2)         => ALT(fuse(List(0), intern(r1)), fuse(List(1), intern(r2)))
  case SEQ(r1, r2)         => SEQ(intern(r1), intern(r2))
  case STAR(r)             => STAR(intern(r))
  case NTIMES(r, n, counter) => NTIMES(intern(r), n, counter)
  case BIT(r, bs)          => BIT(intern(r), bs)
}

def size(r: Rexp): Int = r match {
  case ZERO            => 1
  case ONE             => 1
  case CHAR(_, _)      => 1
  case BIT(r, _)       => size(r)
  case ALT(r1, r2)     => 1 + size(r1) + size(r2)
  case SEQ(r1, r2)     => 1 + size(r1) + size(r2)
  case STAR(r)         => 1 + size(r)
  case NTIMES(r, _, _) => 1 + size(r)
}

// -----------------------
// Syntactic Sugar and Pretty-Printing
// -----------------------

def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil      => ONE
  case c :: Nil => CHAR(c, false)
  case c :: s   => SEQ(CHAR(c, false), charlist2rexp(s))
}
given Conversion[String, Rexp] = s => charlist2rexp(s.toList)

extension (r: Rexp) {
  def |(s: Rexp) = ALT(r, s)
  def %( )      = STAR(r)
  def ~(s: Rexp) = SEQ(r, s)
}

def implode(ss: Seq[String]) = ss.mkString("\n")
def explode(s: String) = s.split("\n").toList

def lst(s: String): String = explode(s) match {
  case hd :: tl => implode(" └" ++ hd :: tl.map("  " ++ _))
  case Nil      => ""
}

def mid(s: String): String = explode(s) match {
  case hd :: tl => implode(" ├" ++ hd :: tl.map(" │" ++ _))
  case Nil      => ""
}

def indent(ss: Seq[String]): String = ss match {
  case init :+ last => implode(init.map(mid) :+ lst(last))
  case _            => ""
}

def pp(e: Rexp): String = e match {
  case ZERO            => "0\n"
  case ONE             => "1\n"
  case CHAR(c, marked) => s"$c${if (marked) " (marked)" else ""}\n"
  case BIT(r, bs)      => s"BIT {bs=$bs}\n" + pp(r)
  case ALT(r1, r2)     => "ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2)     => "SEQ\n" ++ pps(r1, r2)
  case STAR(r)         => "STAR\n" ++ pps(r)
  case NTIMES(r, n, counter) => s"NTIMES{$n} {counter=$counter}\n" ++ pps(r)
}
def pps(es: Rexp*) = indent(es.map(pp))

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

// -----------------------
// Testing
// -----------------------

@main
def test1() = {
  val rexp = intern(STAR(ALT(ALT("a", "b"), "c")))
  println("=============== Test ===============")
  val s = "abc".toList
  println(s"String: $s\n")
  val finReg = matcher2(rexp, s)
  println(s"Original size=${size(rexp)} Result= ${fin(finReg)} \n")
  for (i <- s.indices) {
    println(s"${i + 1}- =shift ${s(i)}=")
    val sPart = s.take(i + 1)
    println(pp(mat(rexp, sPart)))
  }
  println("\n=============== Final Reg ===============")
  println(s"Size=${size(finReg)} , Tree= \n${pp(finReg)}\n")
  println("\n=============== bitcodes ===============")
  val mkepsValue = mkeps(finReg)
  println(s"mkeps= $mkepsValue")
  val decodeValue = decode(rexp, mkepsValue)
  println(s"decode=$decodeValue")
}

@main
def test2@@() = {
  val rexp = intern(ALT(ALT("a", "b"), "c"))
  println("=============== Test ===============")
  val s = "abc".toList
  println(s"String: $s\n")
  val finReg = matcher2(rexp, s)
  println(s"Original size=${size(rexp)} Result= ${fin(finReg)} \n")
  for (i <- s.indices) {
    println(s"${i + 1}- =shift ${s(i)}=")
    val sPart = s.take(i + 1)
    println(pp(mat(rexp, sPart)))
  }
  println("\n=============== Final Reg ===============")
  println(s"Size=${size(finReg)} , Tree= \n${pp(finReg)}\n")
  println("\n=============== bitcodes ===============")
  val mkepsValue = mkeps(finReg)
  println(s"mkeps= $mkepsValue")
  val decodeValue = decode(rexp, mkepsValue)
  println(s"decode=$decodeValue")
}

val EVIL2 = SEQ(STAR(STAR(CHAR('a', false))), CHAR('b', false))
@main
def test2() = {
  val i = 1000
  println(f"$i: ${time_needed(2, matcher(EVIL2, ("a" * i).toList))}%.5f")
} 
}

```



#### Error stacktrace:

```

```
#### Short summary: 

dotty.tools.dotc.core.TypeError$$anon$1: Toplevel definition EVIL2 is defined in
  <WORKSPACE>/play_point3_BIT.sc
and also in
  <WORKSPACE>/.ammonite/scala-2.13.14/amm-3.0.0-2-6342755f/play_point3_BIT/src/ammonite/$file/play_point3_BIT.amm.sc.scala
One of these files should be removed from the classpath.