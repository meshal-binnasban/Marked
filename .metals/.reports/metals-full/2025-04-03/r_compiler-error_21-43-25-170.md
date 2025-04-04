file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/test.sc
### java.lang.NullPointerException: Cannot invoke "scala.meta.internal.pc.CompilerWrapper.compiler()" because "access" is null

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/test.sc
text:
```scala

import $file.enumerate, enumerate.{decode as enumDecode, CDATA}
import $file.regenerate, regenerate._
import $file.rexp, rexp._
import rexp.Rexp.*


val alphabet: LazyList[Char] = LazyList('a', 'b', 'c')

// Bring in the CDATA for Rexp construction
given rexp_cdata: CDATA[Rexp] = List(


  (0, _ => CHAR('a')),
  (0, _ => CHAR('b')),
  (0, _ => CHAR('c')),
  (2, cs => ALT(cs(0), cs(1))),
  (2, cs => SEQ(cs(0), cs(1)))
)

//  (0, _ => ONE),
//  (0, _ => ZERO),
//  (1, cs => STAR(cs(0))),
//

val numRegexes = 100
val maxStringsPerRegex = 5

@main
def runCombinedTests(): Unit = {
  println(s"Testing first $numRegexes regexes with up to $maxStringsPerRegex matching strings each\n")

  for (i <- 0 to numRegexes) {
    val r = enumDecode(i)
    val strings = generate_up_to(alphabet)(10)(r).take(maxStringsPerRegex).toList

    println(s"\n[$i] Regex: $r")
    println(pp(r))
    println(s"strings=$strings")
    
    for ((s, index) <- strings.zipWithIndex) {
      val sList = s.toList
      val finReg= matcher2(r, sList)
      val bitcode=mkfin(finReg)
      val result=fin(finReg)
      println(s"input=$s , Result=$result")
      /* val result = try matcher2(intern2(r), sList) catch {
        case e: Throwable => s"Error: ${e.getMessage}"
      } */

     // println(f"  [$index] Input: '$s' => Match Result: $result")
    }
  }


}
```



#### Error stacktrace:

```
dotty.tools.pc.ScalaPresentationCompiler.semanticdbTextDocument$$anonfun$1(ScalaPresentationCompiler.scala:240)
```
#### Short summary: 

java.lang.NullPointerException: Cannot invoke "scala.meta.internal.pc.CompilerWrapper.compiler()" because "access" is null