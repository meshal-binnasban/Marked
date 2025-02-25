
package ammonite
package $file.^
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


object semiring{
/*<script>*/trait Semiring[S] {
  def zero: S   // Additive identity
  def one: S    // Multiplicative identity
  def plus(a: S, b: S): S  // ⊕ Addition
  def times(a: S, b: S): S // ⊗ Multiplication
}


// Boolean Semiring for standard matching
given booleanSemiring: Semiring[Boolean] with {
  def zero: Boolean = false
  def one: Boolean = true
  def plus(a: Boolean, b: Boolean): Boolean = a || b
  def times(a: Boolean, b: Boolean): Boolean = a && b
}

// Integer Semiring for counting matches
given intSemiring: Semiring[Int] with {
  def zero: Int = 0
  def one: Int = 1
  def plus(a: Int, b: Int): Int = a + b
  def times(a: Int, b: Int): Int = a * b
}

enum Rexp {
  case ZERO
  case ONE
  case CHAR(c: Char, marked: Boolean = false)
  case ALT(r1: Rexp, r2: Rexp)
  case SEQ(r1: Rexp, r2: Rexp)
  case STAR(r: Rexp)
  case NTIMES(r: Rexp, n: Int)
}

import Rexp._

def shift[S](mark: S, r: Rexp, c: Char)(using semiring: Semiring[S]): Rexp = r match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(ch, marked) => CHAR(ch, semiring.times(mark, if (ch == c) semiring.one else semiring.zero) != semiring.zero)
  case ALT(r1, r2) => ALT(shift(mark, r1, c), shift(mark, r2, c))
  case SEQ(r1, r2) =>
    SEQ(
      shift(mark, r1, c),
      shift(
        semiring.plus(semiring.times(mark, if (nullable(r1)) semiring.one else semiring.zero), 
        if (fin(r1)) semiring.one else semiring.zero), r2, c)
    )
  case STAR(r) => STAR(shift(semiring.plus(mark, if (fin(r)) semiring.one else semiring.zero), r, c))
  case NTIMES(r, n) =>
    if (n == 0) ONE
    else SEQ(shift(semiring.plus(mark, if (fin(r)) semiring.one else semiring.zero), r, c), NTIMES(r, n - 1))
}

def nullable(r: Rexp): Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_, _) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, i) => if (i == 0) true else nullable(r)
}

def fin(r: Rexp): Boolean = r match {
  case ZERO => false
  case ONE => false
  case CHAR(_, marked) => marked
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
  case NTIMES(r, _) => fin(r)
}

def matcher[S](r: Rexp, s: List[Char])(using semiring: Semiring[S]): S = {
  s.foldLeft(shift(semiring.one, r, s.head)) { (acc, c) => shift(semiring.zero, acc, c) } match {
    case r => if (fin(r)) semiring.one else semiring.zero
  }
}


@main
def test1() = {
val regex = CHAR('a')

  // Boolean Matching
  val result1 = matcher(regex, List('a'))(using booleanSemiring)
  println(s"Boolean Matching Result: $result1") // Should return true

  // Counting Matches
  val result2 = matcher(regex, List('a'))(using intSemiring)
  println(s"Number of Matches: $result2") // Should return the number of ways the regex matches the string
}/*</script>*/ /*<generated>*/
def $main() = { _root_.scala.Iterator[String]() }
  override def toString = "semiring"
  /*</generated>*/
}
