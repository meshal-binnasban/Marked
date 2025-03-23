import scala.language.implicitConversions
import os.size

// New enum with separate BITS constructor.
enum Rexp {
  case ZERO 
  case ONE 
  case CHAR(c: Char)
  case POINT(r: Rexp, tag: List[Int])
  case ALT(r1: Rexp, r2: Rexp)
  case SEQ(r1: Rexp, r2: Rexp)
  case STAR(r: Rexp)
  case NTIMES(r: Rexp, n: Int, counter: Int)
  case BITS(r: Rexp, bits: List[Int])
}
import Rexp._

// --- Helper functions for bitcode management ---

// fuse attaches new bit codes to an Rexp, wrapping it in a BITS node.
def fuse(cs: List[Int], r: Rexp): Rexp = r match {
  case BITS(inner, bits) => BITS(inner, bits ++ cs)
  case _ => BITS(r, cs)
}

// --- Basic Rexp functions ---

// Nullability is independent of bitcode wrappers.
def nullable(r: Rexp): Boolean = r match {
  case ZERO                     => false
  case ONE                      => true
  case CHAR(_)                  => false
  case POINT(inner, _)          => nullable(inner)
  case ALT(r1, r2)              => nullable(r1) || nullable(r2)
  case SEQ(r1, r2)              => nullable(r1) && nullable(r2)
  case STAR(_)                  => true
  case NTIMES(r, n, _)          => if(n == 0) true else nullable(r)
  case BITS(inner, _)           => nullable(inner)
}

// fin: tells whether the expression is “final” (i.e. it carries a marker)
def fin(r: Rexp): Boolean = r match {
  case ZERO          => false
  case ONE           => false
  case CHAR(_)       => false
  case POINT(inner, tag) =>
    inner match {
      case CHAR(_) =>
        if(tag.nonEmpty && tag.head > 0) true else false
      case _ => fin(inner)
    }
  case ALT(r1, r2)   => fin(r1) || fin(r2)
  case SEQ(r1, r2)   => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r)       => fin(r)
  case NTIMES(r, n, counter) => counter == n && fin(r)
  case BITS(inner, _) => fin(inner)
}

// --- shift function ---
// This function “shifts” the regex with a given character and position.
// It may add bit information in several ways. In cases where a bit is to be attached,
// we now use POINT (or fuse via BITS) rather than carrying an extra parameter.
def shift(m: Boolean, re: Rexp, cp: (Char, Int)): Rexp = {
  val (c, pos) = cp
  re match {
    case ZERO => ZERO
    case ONE  => ONE
    case CHAR(d) =>
      if(m && d == c) POINT(CHAR(d), List(pos+1))
      else CHAR(d)
    case POINT(CHAR(d), tag) =>
      if(m && d == c) POINT(CHAR(d), pos+1 :: tag)
      else POINT(CHAR(d), 0 :: tag)
    case POINT(inner, tag) =>
      // For non-CHAR inside a POINT, shift the inner expression.
      POINT(shift(m, inner, cp), tag)
    case ALT(r1, r2) =>
      ALT(shift(m, r1, cp), shift(m, r2, cp))
    case SEQ(r1, r2) =>
      SEQ(shift(m, r1, cp), shift((m && nullable(r1)) || fin(r1), r2, cp))
    case STAR(r) =>
      // In the STAR case we “fuse” bits computed from mkeps(r) and a trailing 0.
      val shifted = STAR(shift(m || fin(r), r, cp))
      fuse(mkeps(r) ++ List(0), shifted)
    case NTIMES(r, n, counter) =>
      if (counter == n) NTIMES(r, n, counter)
      else {
        if(m || fin(r))
          NTIMES(shift(m || fin(r), r, cp), n, counter+1)
        else
          NTIMES(shift(false, r, cp), n, counter)
      }
    case BITS(r, bits) =>
      // Propagate the bit information: shift the inner Rexp then reattach the bits.
      fuse(bits, shift(m, r, cp))
  }
}

// --- Matching functions ---
def mat(r: Rexp, s: List[Char]): Rexp = s match {
  case Nil => r
  case c :: cs =>
    cs.zipWithIndex.foldLeft(shift(true, r, (c, 0))) {
      case (currRexp, (c, i)) => shift(false, currRexp, (c, i + 1))
    }
}

def matcher(r: Rexp, s: List[Char]): Boolean =
  if (s.isEmpty) nullable(r) else fin(mat(r, s))

def matcher2(r: Rexp, s: List[Char]): Rexp =
  if (s.isEmpty)
     if(nullable(r)) r else ZERO
  else mat(r, s)

// --- mkeps ---
// Computes the bitcode that represents an “empty match” for the regex.
// In this version we propagate any bits stored in a BITS wrapper.
def mkeps(r: Rexp): List[Int] = r match {
  case ONE           => List()
  case CHAR(_)       => List()
  case POINT(inner, tag) => tag
  case ALT(r1, r2)   =>
    if (fin(r1)) mkeps(r1)
    else if (fin(r2)) mkeps(r2)
    else List()
  case SEQ(r1, r2)   =>
    if (fin(r1) && nullable(r2)) mkeps(r1) ++ mkeps(r2)
    else if (fin(r2)) mkeps(r2)
    else List()
  case STAR(r)       =>
    if (fin(r)) mkeps(r) ++ List(1)
    else List()
  case NTIMES(r, n, counter) =>
    if (counter == n && fin(r)) mkeps(r)
    else List()
  case ZERO          => List()
  case BITS(r, bits) => bits ++ mkeps(r)
}

// --- decode ---
// Translates the bitcodes into a VALUE representation.
// We “unwrap” BITS by prepending its bit list.
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
  case ONE          => (ONEV, bs)
  case CHAR(c)      => (CHARV(c), bs)
  case POINT(inner, tag) =>
    // We ignore the tag here and simply decode the inner expression.
    decode(inner, bs)
  case ALT(r1, r2)  => bs match {
      case 0 :: bs1 =>
        val (v, bsp) = decode(r1, bs1)
        (LEFT(v), bsp)
      case 1 :: bs1 =>
        val (v, bsp) = decode(r2, bs1)
        (RIGHT(v), bsp)
      case _ =>
        // Fallback if bitcode is not as expected.
        (ZEROV, bs)
    }
  case SEQ(r1, r2)  =>
    val (v1, bs2) = decode(r1, bs)
    val (v2, bs3) = decode(r2, bs2)
    (SEQV(v1, v2), bs3)
  case STAR(r)      => bs match {
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
    // For NTIMES, we simply decode the inner expression.
    decode(r, bs)
  case BITS(r, bits) =>
    // Prepend the stored bits before decoding.
    decode(r, bits ++ bs)
}

// --- Other utility functions ---

def popPoints(r: Rexp): (Rexp, Rexp) = r match {
  case ZERO => (ZERO, ZERO)
  case ONE  => (ONE, ONE)
  case CHAR(c) => (CHAR(c), CHAR(c))
  case POINT(inner, tags) =>
    val (currentInner, nextInner) = popPoints(inner)
    if (tags.isEmpty) {
      (currentInner, nextInner)
    } else {
      val updatedNext = POINT(inner, tags.tail)
      tags.head match {
        case 0 => (currentInner, updatedNext)
        case x => (POINT(currentInner, List(x)), updatedNext)
      }
    }
  case ALT(r1, r2) =>
    val (curr1, next1) = popPoints(r1)
    val (curr2, next2) = popPoints(r2)
    (ALT(curr1, curr2), ALT(next1, next2))
  case SEQ(r1, r2) =>
    val (curr1, next1) = popPoints(r1)
    val (curr2, next2) = popPoints(r2)
    (SEQ(curr1, curr2), SEQ(next1, next2))
  case STAR(inner) =>
    val (curr, next) = popPoints(inner)
    (STAR(curr), STAR(next))
  case NTIMES(inner, n, counter) =>
    val (curr, next) = popPoints(inner)
    (NTIMES(curr, n, counter), NTIMES(next, n, counter))
  case BITS(r, bits) =>
    // Apply popPoints to the inner expression and reattach the bits.
    val (curr, next) = popPoints(r)
    (fuse(bits, curr), fuse(bits, next))
}

def traverseStages(r: Rexp, inputLength: Int): List[Rexp] = {
  def loop(state: Rexp, stages: List[Rexp], remaining: Int): List[Rexp] = {
    if (remaining == 0) stages
    else {
      val (currentStage, nextState) = popPoints(state)
      loop(nextState, stages :+ currentStage, remaining - 1)
    }
  }
  loop(r, List.empty, inputLength)
}

def traverseStages2(r: Rexp, inputLength: Int): List[Rexp] = {
  val (stages, _) = (0 until inputLength).foldLeft((List[Rexp](), r)) {
    case ((acc, state), _) =>
      val (currentStage, nextState) = popPoints(state)
      (acc :+ currentStage, nextState)
  }
  stages
}

def extractPoints(r: Rexp): List[(Rexp, Int)] = r match {
  case POINT(r, pos) => pos.filter(_ != 0).map(p => (r, p-1)).reverse
  case ALT(r1, r2)   => extractPoints(r1) ++ extractPoints(r2)
  case SEQ(r1, r2)   => extractPoints(r1) ++ extractPoints(r2)
  case STAR(r)       => extractPoints(r)
  case NTIMES(r, _, _) => extractPoints(r)
  case BITS(r, _)    => extractPoints(r)
  case _             => List()
}

def matchCount(r: Rexp): Int = r match {
  case ZERO      => 0
  case ONE       => 0
  case CHAR(_)   => 1
  case POINT(r, _)  => matchCount(r)
  case ALT(r1, r2)  => math.max(matchCount(r1), matchCount(r2))
  case SEQ(r1, r2)  => matchCount(r1) + matchCount(r2)
  case STAR(r)      => matchCount(r)
  case NTIMES(r, n, _) => n * matchCount(r)
  case BITS(r, _) => matchCount(r)
}

def size(r: Rexp): Int = r match {
  case ZERO        => 1
  case ONE         => 1
  case CHAR(_)     => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r)     => 1 + size(r)
  case NTIMES(r, _, _) => 1 + size(r)
  case POINT(r, _) => 1 + size(r)
  case BITS(r, _)  => 1 + size(r)
}

// --- Syntax sugar and pretty printing ---

def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil    => ONE
  case c :: Nil => CHAR(c)
  case c :: s => SEQ(CHAR(c), charlist2rexp(s))
}
given Conversion[String, Rexp] = s => charlist2rexp(s.toList)

extension (r: Rexp) {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

// Pretty-printing functions
def implode(ss: Seq[String]) = ss.mkString("\n")
def explode(s: String) = s.split("\n").toList

def lst(s: String): String = explode(s) match {
  case hd :: tl => implode(" └" +: (hd :: tl.map("  " + _)))
  case Nil      => ""
}

def mid(s: String): String = explode(s) match {
  case hd :: tl => implode(" ├" +: (hd :: tl.map(" │" + _)))
  case Nil      => ""
}

def indent(ss: Seq[String]): String = ss match {
  case init :+ last => implode(init.map(mid) :+ lst(last))
  case _            => ""
}

def pp(e: Rexp): String = e match {
  case ZERO         => "0\n"
  case ONE          => "1\n"
  case CHAR(c)      => s"$c"
  case POINT(CHAR(c), tag) =>
    if(tag.nonEmpty && tag.head > 0) s"•$c {tag=$tag}"
    else s"$c {tag=$tag}"
  case POINT(r, tag) => pp(r)  // delegate to inner
  case ALT(r1, r2)   => s"ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2)   => s"SEQ\n" ++ pps(r1, r2)
  case STAR(r)       => "STAR\n" ++ pps(r)
  case NTIMES(r, n, counter) => s"NTIMES{$n} {counter=$counter}\n" ++ pps(r)
  case BITS(r, bits) => s"BITS {bits=$bits}\n" ++ pps(r)
}
def pps(es: Rexp*): String = indent(es.map(pp))

// --- intern ---
// Interns a regex by “pushing in” bit markers on alternatives.
def intern(r: Rexp): Rexp = r match {
  case ZERO          => ZERO
  case ONE           => ONE
  case CHAR(c)       => CHAR(c)
  case ALT(r1, r2)   => ALT(fuse(List(0), intern(r1)), fuse(List(1), intern(r2)))
  case SEQ(r1, r2)   => SEQ(intern(r1), intern(r2))
  case STAR(r)       => STAR(intern(r))
  case NTIMES(r, n, counter) => NTIMES(intern(r), n, counter)
  case POINT(r, tag) => POINT(intern(r), tag)
  case BITS(r, bits) => fuse(bits, intern(r))
}

// --- Test functions ---
// testing bitcodes
@main
def test1() = {
    val rexp = intern(STAR( ALT( ALT("a","b") , "c" ) ))

    println("=============== Test ===============")
    val s="abc".toList
    println(s"String: $s\n")
    val finReg=matcher2(rexp, s)
    println(s"Original size=${size(rexp)} Result= ${fin(finReg)} \n")

    for (i <- s.indices) {
    println(s"${i + 1}- =shift ${s(i)}=")
    val sPart = s.take(i + 1)
    println(pp(mat(rexp, sPart)))
    }

    println("\n=============== Final Reg ===============n")
    println(s"Size=${size(finReg)} , Tree= \n ${pp(finReg)}\n")
    println("\n=============== bitcodes ===============n")

    val mkepsValue = mkeps(finReg)
    println(s"mkeps= $mkepsValue")
    val decodeValue=decode(rexp,mkepsValue)
    println(s"decode=$decodeValue")  

   /*  println("\n=============== EVIL ===============n")

    val EVIL2 = STAR(STAR("a" | "b" ))

    val finReg2=matcher2(EVIL2, "aa".toList)
    println(pp(finReg2))
    val mkepsEvilValue = mkeps(finReg2)
    println(s"mkeps= $mkepsEvilValue")
    val decodeEvilValue=decode(EVIL2,mkepsEvilValue)
    println(s"decode=$decodeEvilValue") */
}

val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

@main
def test2() = {
 // for (i <- 0 to 7000000 by 500000) {
 // }
 //:+ 'b'
  val i=1000
  println(f"$i: ${time_needed(2, matcher(EVIL2, ("a" * i).toList))}%.5f")

} 

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

