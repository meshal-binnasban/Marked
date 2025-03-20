
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


object derivativesBitcode{
/*<script>*/abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 
case class NTIMES(r: Rexp, n: Int) extends Rexp 

abstract class BRegExp
case class BZERO() extends BRegExp
case class BONE(bs: List[Int]) extends BRegExp
case class BCHAR(bs: List[Int], c: Char) extends BRegExp
case class BALTs(bs: List[Int], r1: BRegExp, r2: BRegExp) extends BRegExp
case class BSEQ(bs: List[Int], r1: BRegExp, r2: BRegExp) extends BRegExp
case class BSTAR(bs: List[Int], r: BRegExp) extends BRegExp
case class BNTIMES(bs: List[Int], r: BRegExp, n: Int) extends BRegExp

// Convert standard regex to bitcoded regex
def fuse(bs: List[Int], r: BRegExp): BRegExp = r match {
  case BZERO() => BZERO()
  case BONE(bs2) => BONE(bs ++ bs2)
  case BCHAR(bs2, c) => BCHAR(bs ++ bs2, c)
  case BALTs(bs2, r1, r2) => BALTs(bs ++ bs2, r1, r2)
  case BSEQ(bs2, r1, r2) => BSEQ(bs ++ bs2, r1, r2)
  case BSTAR(bs2, r) => BSTAR(bs ++ bs2, r)
  case BNTIMES(bs2, r, n) => BNTIMES(bs ++ bs2, r, n)
}

def internalize(r: Rexp): BRegExp = r match {
  case ZERO => BZERO()
  case ONE => BONE(List())
  case CHAR(c) => BCHAR(List(), c)
  case ALT(r1, r2) => BALTs(List(), fuse(List(0), internalize(r1)), fuse(List(1), internalize(r2)))
  case SEQ(r1, r2) => BSEQ(List(), internalize(r1), internalize(r2))
  case STAR(r) => BSTAR(List(), internalize(r))
  case NTIMES(r, n) => BNTIMES(List(), internalize(r), n)
}

// Nullable function
def bnullable(r: BRegExp): Boolean = r match {
  case BZERO() => false
  case BONE(_) => true
  case BCHAR(_, _) => false
  case BALTs(_, r1, r2) => bnullable(r1) || bnullable(r2)
  case BSEQ(_, r1, r2) => bnullable(r1) && bnullable(r2)
  case BSTAR(_, _) => true
  case BNTIMES(_, r, n) => if (n == 0) true else bnullable(r)
}

// Compute derivative with bitcodes
def bder(c: Char, r: BRegExp): BRegExp = r match {
  case BZERO() => BZERO()
  case BONE(_) => BZERO()
  case BCHAR(bs, d) => if (c == d) BONE(bs) else BZERO()
  case BALTs(bs, r1, r2) => BALTs(bs, bder(c, r1), bder(c, r2))
  case BSEQ(bs, r1, r2) => 
    if (bnullable(r1)) BALTs(bs, BSEQ(List(), bder(c, r1), r2), fuse(bmkeps(r1), bder(c, r2)))
    else BSEQ(bs, bder(c, r1), r2)
  case BSTAR(bs, r) => BSEQ(bs :+ 0, bder(c, r), BSTAR(List(), r))
  case BNTIMES(bs, r, n) => 
    if (n == 0) BZERO()
    else BSEQ(bs :+ 0, bder(c, r), BNTIMES(List(), r, n - 1))
}

// Compute derivative with respect to a string
def bders(s: List[Char], r: BRegExp): BRegExp = s match {
  case Nil => r
  case c :: cs => bders(cs, bder(c, r))
}

// Extract the bitcode
def bmkeps(r: BRegExp): List[Int] = r match {
  case BONE(bs) => bs
  case BALTs(bs, r1, r2) => bs ++ (if (bnullable(r1)) bmkeps(r1) else bmkeps(r2))
  case BSEQ(bs, r1, r2) => bs ++ bmkeps(r1) ++ bmkeps(r2)
  case BSTAR(bs, _) => bs :+ 1
  case BNTIMES(bs, r, n) => if (n == 0) bs :+ 1 else bs ++ List(0) ++ bmkeps(r) ++ bmkeps(BNTIMES(List(), r, n - 1))
}

// Decode function to reconstruct match structure
def decode(bs: List[Int], r: Rexp): (String, List[Int]) = r match {
  case ONE => ("", bs)
  case CHAR(c) => (c.toString, bs)
  case ALT(r1, r2) => bs match {
    case 0 :: rest => val (v, rem) = decode(rest, r1); (s"Left($v)", rem)
    case 1 :: rest => val (v, rem) = decode(rest, r2); (s"Right($v)", rem)
    case _ => ("", bs)
  }
  case SEQ(r1, r2) =>
    val (v1, bs1) = decode(bs, r1)
    val (v2, bs2) = decode(bs1, r2)
    (s"Seq($v1, $v2)", bs2)
  case STAR(r) => bs match {
    case 1 :: rest => ("Stars([])", rest)
    case 0 :: rest => 
      val (v, bs1) = decode(rest, r)
      val (starsRest, bs2) = decode(bs1, STAR(r))
      (s"Stars($v, $starsRest)", bs2)
    case _ => ("", bs)
  }
}

// Matcher function
def bmatcher(r: Rexp, s: String): Boolean = {
  val br = internalize(r)
  val finalR = bders(s.toList, br)
  bnullable(finalR)
}



@main
def test1() = {
// Run tests
val regex = STAR(ALT(CHAR('a'), CHAR('b')))
val input = "abab"
val br = internalize(regex)
val finalR = bders(input.toList, br)
val bitcode = bmkeps(finalR)
val decoded = decode(bitcode, regex)

println(s"Matched: ${bnullable(finalR)}")
println(s"Bitcode: $bitcode")
println(s"Decoded structure: ${decoded._1}")


}/*</script>*/ /*<generated>*/
def $main() = { _root_.scala.Iterator[String]() }
  override def toString = "derivativesBitcode"
  /*</generated>*/
}
