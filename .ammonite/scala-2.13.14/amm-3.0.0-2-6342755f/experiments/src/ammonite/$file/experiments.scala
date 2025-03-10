
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


object experiments{
/*<script>*/sealed trait Regex
case object ZERO extends Regex
case object ONE extends Regex
case class CHAR(c: Char, marked: Boolean = false) extends Regex
case class ALT(r1: Regex, r2: Regex) extends Regex
case class SEQ(r1: Regex, r2: Regex) extends Regex
case class STAR(r: Regex) extends Regex
case class NTIMES(r: Regex, n: Int, count: Int = 0, inner: Option[Regex] = None) extends Regex

object MarkedRegexMatcher {

  def nullable(r: Regex): Boolean = r match {
    case ALT(r1, r2)    => nullable(r1) || nullable(r2)
    case SEQ(r1, r2)    => nullable(r1) && nullable(r2)
    case STAR(_)        => true
    case NTIMES(r, n)   => if (n == 0) true else nullable(r) && nullable(NTIMES(r, n - 1))
    case CHAR(_, _)     => false
    case ONE            => true
    case ZERO           => false
  }

  def fin(r: Regex): Boolean = r match {
    case CHAR(_, marked) => marked
    case ALT(r1, r2)     => fin(r1) || fin(r2)
    case SEQ(r1, r2)    => (fin(r1) && nullable(r2)) || fin(r2)
    case STAR(r)         => fin(r)
    case NTIMES(_, n, count) => count == n
    case _ => false
  }

  def shift(m: Boolean, c: Char, r: Regex): Regex = r match {
    case CHAR(d, _) => CHAR(d, m && c == d)
    case ALT(r1, r2) => ALT(shift(m, c, r1), shift(m, c, r2))
    case SEQ(r1, r2) => SEQ(shift(m, c, r1), shift(m && nullable(r1) || fin(r1), c, r2))
    case STAR(r1) => STAR(shift(m || fin(r1), c, r1))

    case NTIMES(r1, n, count) =>
      if (count == n) NTIMES(r1, n, count)
      else {
        val shifted_r = shift(m, c, r1)
        if (fin(shifted_r)) NTIMES(r, n, count + 1)
        else NTIMES(shifted_r, n, count)
      }

    case other => other
  }

  def matches(r: Regex, s: String): Boolean = {
    val finalR = s.foldLeft(r)((acc, c) => shift(true, c, acc))
    nullable(finalShift(finalUnmark(finalR))) || fin(finalR(finalShift(r, s)))
  }

  private def shifted_r = ??? // fix according to above logic

  // TEST CASE
  def main(args: Array[String]): Unit = {
    val testRexp = SEQ(NTIMES(ALT(CHAR('a'), ONE), 2), NTIMES(CHAR('a'), 2))
    val testString = "aaaa"
    val result = matches(testRexp, testString)

    println(s"Regex matches 'aaaa': $result")
  }
}
/*</script>*/ /*<generated>*/
def $main() = { _root_.scala.Iterator[String]() }
  override def toString = "experiments"
  /*</generated>*/
}
