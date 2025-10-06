file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts_Sets.sc
### scala.MatchError: TypeDef(Marks,AppliedTypeTree(Ident(Set),List(Ident(String)))) (of class dotty.tools.dotc.ast.Trees$TypeDef)

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 96
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/matchers-comparison/Shifts_Sets.sc
text:
```scala
import scala.language.implicitConversions
import $file.rexp, rexp._

type Marks = Set[String]

v@@
// shifts function
def shifts(ms: Marks, r: Rexp): Marks = r match {
  case ZERO => Set()
  case ONE  => Set()
  case CHAR(c) => for (m <- ms; if m != "" && m.head == c) yield m.tail
  case ALT(r1, r2) => shifts(ms, r1) ++ shifts(ms, r2)
  case SEQ(r1, r2) => {
    val ms1 = shifts(ms, r1)
    (nullable(r1), nullable(r2)) match {
      case (true,  true)  => shifts(ms1 ++ ms, r2) ++ ms1
      case (true,  false) => shifts(ms1 ++ ms, r2)
      case (false, true)  => shifts(ms1, r2) ++ ms1
      case (false, false) => shifts(ms1, r2)
    }
  }
  case STAR(r) => {
    val ms1 = shifts(ms, r)
    if (ms1 == Set()) ms1 else ms1 ++ shifts(ms1, STAR(r))
  }
  case NTIMES(r, n) =>
    if (n == 0) ms
    else {
      val ms1 = shifts(ms, r)
      println(s"ms1= $ms1")
      if (ms1 == Set()) ms1
      else if (nullable(r)) ms1 ++ shifts(ms1, NTIMES(r, n - 1))
      else shifts(ms1, NTIMES(r, n - 1))
    }
}

// the main matching function 
def matcher(r: Rexp, s: String): Boolean =
  if (s == "") nullable(r)
  else shifts(Set(s), r).contains("")

def mat(r: Rexp, s: String): Marks = shifts(Set(s), r)


@main
def test1() = {
  println("=====Test====")
  val r = NTIMES("a",2)
    //(("a") | (ONE)) ~ (("a") | NTIMES("a",3))
    //aaa
    
  val s = "aa"
  println("=string=")
  println(s)
  //println(matcher(r,s))
  println(mat(r,s))
}

@main
def test3() = {
  println("=====Test====")
  val r = %( %( "a" ) | %( "aa" ) | %( "aaa" ) | %( "aaaa" ) | %( "aaaaa" ) )
  val s = "a" * 1000
  println("=string=")
  println(s)
  println(matcher(r,s))
}
 



```



#### Error stacktrace:

```
dotty.tools.pc.completions.KeywordsCompletions$.checkTemplateForNewParents$$anonfun$2(KeywordsCompletions.scala:218)
	scala.Option.map(Option.scala:242)
	dotty.tools.pc.completions.KeywordsCompletions$.checkTemplateForNewParents(KeywordsCompletions.scala:215)
	dotty.tools.pc.completions.KeywordsCompletions$.contribute(KeywordsCompletions.scala:44)
	dotty.tools.pc.completions.Completions.completions(Completions.scala:126)
	dotty.tools.pc.completions.CompletionProvider.completions(CompletionProvider.scala:139)
	dotty.tools.pc.ScalaPresentationCompiler.complete$$anonfun$1(ScalaPresentationCompiler.scala:150)
```
#### Short summary: 

scala.MatchError: TypeDef(Marks,AppliedTypeTree(Ident(Set),List(Ident(String)))) (of class dotty.tools.dotc.ast.Trees$TypeDef)