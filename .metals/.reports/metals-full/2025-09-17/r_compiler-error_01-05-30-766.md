file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts.sc
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 1221
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts.sc
text:
```scala
import scala.language.implicitConversions
import $file.rexp, rexp._

type Marks = List[String]

// shifts function 
def shifts(ms: Marks, r: Rexp) : Marks = r match {
      case ZERO => Nil
      case ONE => Nil
      case CHAR(c) => for (m <- ms; if m != "" && m.head == c) yield m.tail
      case ALT(r1, r2) => (shifts(ms, r1) ::: shifts(ms, r2))
      case SEQ(r1, r2) => {
        val ms1 = shifts(ms, r1).prune2
        (nullable(r1), nullable(r2)) match {
          case (true, true) =>  (shifts((ms1 ::: ms), r2) ::: ms1)
          case (true, false) => shifts((ms1 ::: ms.prune2), r2) 
          case (false, true) => (shifts(ms1, r2) ::: ms1)
          case (false, false) => shifts(ms1, r2)
        }
      }
      case STAR(r) => {
        val ms1 = shifts(ms, r)
        if(ms1.isEmpty) Nil 
        else
        (ms1 ::: shifts(ms1, STAR(r)))
      }
      case NTIMES(r,n) if n == 0 => ms
    //case NTIMES(r,n) if n < 0 => Nil
      case NTIMES(r,n) =>
        if((ms.exists(_ == "") && n != 0) && !nullable(r)){ 
          Nil
          } else{
            val ms1 = shifts(ms,r)
            if(nullable(r))
            if(ms1.isEmpty)
            {

            }els@@
            ( ms1 ::: shifts(ms1,NTIMES(r,n-1)))
            else
              ( shifts(ms1,NTIMES(r,n-1))   )
              }

}
  

// the main matching function 
def matcher(r: Rexp, s: String) : Boolean = {
  if (s == "") nullable(r)
  else 
    shifts(List(s), r).exists(_ == "")
}

extension (ms: Marks)
  def prune2: Marks =
    var seen = Set.empty[String]
    ms.filter { s =>
      val keep = !seen.contains(s)
      if (keep) seen += s
      keep
}

@main
def test1() = {
  println("=====Test====")
  val r = ((ZERO | "a") | (ONE ~ ONE)) ~ (("a" | "a") | NTIMES("a",3))
    //aaa
    
  val s = "aaa"
  println("=string=")
  println(s)
  println(matcher(r,s))
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