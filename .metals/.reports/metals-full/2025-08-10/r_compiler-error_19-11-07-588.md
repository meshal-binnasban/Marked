file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Test.sc
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 865
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Test.sc
text:
```scala


import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.Derivatives
import $file.Play
import $file.Shifts

import scala.language.implicitConversions


def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

@main
def test1() = {
  println("=====Test====")
  val r = ("a" | "ab") ~ ("c" | "bc")
  val s = "abc"
  println("=string=")
  println(s)

  println(s"Derivatives: ${Derivatives.matcher(r, s)}")
  println(s"Play: ${Play.matcher(r, s)}")
  println(s"Shifts: ${Shifts.matcher(r, s)}")
  val derivatives=time_needed(1000,Derivatives.matcher(r, s))
  val play=time_needed(1000,Play.matcher(r, s))
  val shifts=time_needed(1000,Shifts.matcher(r, s))
  val l=(derivatives,play,shifts)
  l.sortBy(_1@@)
  println(s"=Times=")
  println(s"Derivatives= $derivatives")
  println(s"Play= $play")
  println(s"Shifts= $shifts")

  
}


@main
def testAll() = {
  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE),
        (0, _ => ZERO),
        (0, _ => CHAR('a')),
        (0, _ => CHAR('b')),
        (0, _ => CHAR('c')),
        (1, cs => STAR(cs(0))),
        (2, cs => ALT(cs(0), cs(1))),
        (2, cs => SEQ(cs(0), cs(1)))
      )
  val alphabet = LazyList('a', 'b','c')

  for (i <- (0L to 100_000_0L)) {//100_000_000L
    val r = enumerate.decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "")
      { 
        val derivatives=time_needed(1000,Derivatives.matcher(r, s))
        println(s"${derivatives}%.5f")

      }
  }
}
```



#### Error stacktrace:

```
scala.collection.LinearSeqOps.apply(LinearSeq.scala:129)
	scala.collection.LinearSeqOps.apply$(LinearSeq.scala:128)
	scala.collection.immutable.List.apply(List.scala:79)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:244)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:101)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:88)
	dotty.tools.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:46)
	dotty.tools.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:435)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: -1