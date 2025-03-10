file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/.ammonite/scala-2.13.14/amm-3.0.0-2-6342755f/marked-intNTIMES/src/ammonite/$file/marked-intNTIMES.amm.sc.scala
### dotty.tools.dotc.core.TypeError$$anon$1: Toplevel definition all is defined in
  <WORKSPACE>/marked-intNTIMES.sc
and also in
  <WORKSPACE>/.ammonite/scala-2.13.14/amm-3.0.0-2-6342755f/marked-intNTIMES/src/ammonite/$file/marked-intNTIMES.amm.sc.scala
One of these files should be removed from the classpath.

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 3699
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/.ammonite/scala-2.13.14/amm-3.0.0-2-6342755f/marked-intNTIMES/src/ammonite/$file/marked-intNTIMES.amm.sc.scala
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


object `marked-intNTIMES`{
/*<start>*/
enum Rexp {
  case ZERO
  case ONE 
  case CHAR(c: Char , marked: Int = 0)
  case ALT(r1: Rexp, r2: Rexp )
  case SEQ(r1: Rexp, r2: Rexp)
  case STAR(r: Rexp)
  case NTIMES(r: Rexp, n: Int,nmark:Int =0) 
  
  override def toString: String = this match {
    case ZERO => "ZERO"
    case ONE => "ONE"
    case CHAR(c, marked) => s"CHAR($c)"
    case ALT(r1, r2) => s"ALT($r1, $r2)"
    case SEQ(r1, r2) => s"SEQ($r1, $r2)"
    case STAR(r) => s"STAR($r)"
    case NTIMES(r, n, nmark) => s"NTIMES($r, n=$n, nmark=$nmark)"
  }
}
import Rexp._

def OPT(r: Rexp) = ALT(r, ONE)

def shift(mark: Int,r: Rexp,c: Char ): Rexp = r match {
    case ZERO => ZERO
    case ONE=> ONE
    case CHAR(ch,marked) => CHAR(ch, if(ch==c) 1 * mark else 0 * mark )
    case ALT(r1, r2) => ALT(shift(mark,r1,c),shift(mark,r2,c))
    case SEQ(r1,r2) => SEQ (shift(mark,r1,c), shift(mark * nullable(r1) + fin(r1),r2,c))
    case STAR(r) => STAR(shift(mark + fin(r), r,c))
    case NTIMES(r, n,nmark) =>
        // test this to increament each time r is matched 
      if(n==0) ONE 
      else{

        val rr=shift(mark + fin(r), r,c)
        if(fin(rr) == 1){
            NTIMES(rr,n,nmark+1)
        }
        else{
            NTIMES(rr,n,nmark)
        }
        
      } 
     
}

def nullable(r: Rexp) : Int = r match {
  case ZERO => 0
  case ONE => 1
  case CHAR(_,_) => 0
  case ALT(r1, r2) => nullable(r1) + nullable(r2)
  case SEQ(r1, r2) => nullable(r1) + nullable(r2)
  case STAR(_) => 1
  case NTIMES(r, n ,nmark) => if (n == 0) 1 else nullable(r) //?
}

def fin(r: Rexp) : Int = r match {
  case ZERO => 0
  case ONE => 0
  case CHAR(c,marked) => marked
  case ALT(r1, r2) => fin(r1) + fin(r2)
  case SEQ(r1, r2) => (fin(r1) * nullable(r2)) + fin(r2)
  case STAR(r) => fin(r)
  case NTIMES(r, n,nmark) => if(nmark == n) 1 else 0 // ? if (i == 0) false else
}

def matcher(r: Rexp , s: String) : Int = s.toList match {
  case Nil => nullable(r)
//case c :: cs => fin(cs.foldLeft(shift(true, r, c)) { (acc, c) =>
//  shift(false, acc, c) })
  case c :: cs => fin(cs.foldLeft(shift(1, r, c))(shift(0, _, _)))

}

@main
def test0() = {
    val r = SEQ(SEQ(CHAR('x'), CHAR('y')), CHAR('z'))
    println(matcher(r,"xyz"))

    val n=2
    val rexp=EVIL1(n)
    val s="aa"
    val result=matcher(rexp,s)
    println(s" NTIMES Result=$result")

}

def size(r: Rexp) : Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_,_) => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r) => 1 + size(r)
  case NTIMES(r, _) => 1 + size(r)    
}

def SeqNTIMES(r: Rexp): Rexp = r match {
  case ZERO => ZERO
  case ONE  => ONE
  case CHAR(c, marked) => CHAR(c, marked)
  case ALT(r1, r2) =>
    ALT(SeqNTIMES(r1), SeqNTIMES(r2))
  case SEQ(r1, r2) =>
    SEQ(SeqNTIMES(r1), SeqNTIMES(r2))
  case STAR(r1) =>
    STAR(SeqNTIMES(r1))
  case NTIMES(r, n, _) =>
    val expandR = SeqNTIMES(r)
    if (n <= 0) ONE else
        if (n == 1) 
      @@expandR
else {
      SeqNTIMES(
        SEQ(
          expandedSub,
          NTIMES(expandedSub, n - 1)
        )
      )
    }
}


def EVIL1(n: Int) = SEQ(NTIMES(OPT(CHAR('a')), n), NTIMES(CHAR('a'), n))
val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

// for measuring time
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}


//@arg(doc = "Test (a?{n}) (a{n})")
@main
def test1() = {
  for (i <- 0 to 11000 by 1000) {
    println(f"$i: ${time_needed(2, matcher(EVIL1(i), "a" * i))}%.5f")
  }
}

//@arg(doc = "Test (a*)* b")
@main
def test2() = {
  for (i <- 0 to 7000000 by 500000) {
    println(f"$i: ${time_needed(2, matcher(EVIL2, "a" * i))}%.5f")
  }
} 

//@arg(doc = "All tests.")
@main
def all() = { test1(); test2() } 




 
}

```



#### Error stacktrace:

```

```
#### Short summary: 

dotty.tools.dotc.core.TypeError$$anon$1: Toplevel definition all is defined in
  <WORKSPACE>/marked-intNTIMES.sc
and also in
  <WORKSPACE>/.ammonite/scala-2.13.14/amm-3.0.0-2-6342755f/marked-intNTIMES/src/ammonite/$file/marked-intNTIMES.amm.sc.scala
One of these files should be removed from the classpath.