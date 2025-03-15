file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-intNTIMES.sc
### java.lang.AssertionError: assertion failed: denotation class Int invalid in run 3. ValidFor: Period(4.1-2)

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-intNTIMES.sc
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
import scala.compiletime.ops.string

enum Rexp {
  case ZERO
  case ONE 
  case CHAR(c: Char , marked: List[Int] = List(0)) //marked: Int = 0
  case ALT(r1: Rexp, r2: Rexp )
  case SEQ(r1: Rexp, r2: Rexp)
  case STAR(r: Rexp)
  case NTIMES(r: Rexp, n: Int,nmark:List[Int] = List(0) , counter:Int=0) 
  case INIT(r: Rexp)
  
  override def toString: String = this match {
    case ZERO => "ZERO"
    case ONE => "ONE"
    case CHAR(c, marked) => s"CHAR($c , $marked)"
    case ALT(r1, r2) => s"ALT($r1, $r2)"
    case SEQ(r1, r2) => s"SEQ($r1, $r2)"
    case STAR(r) => s"STAR($r)"
    case NTIMES(r, n, nmark ,counter) => s"NTIMES($r, n=$n, nmark=$nmark , count=$counter)"
    case INIT(r: Rexp) => s"INIT($r)"
  }
}
import Rexp._

def OPT(r: Rexp) = ALT(r, ONE)

def shift(mark: Int,re: Rexp,c: Char ): Rexp = re match {
    
    case ZERO => ZERO
    case ONE=> ONE
    case CHAR(ch,marked) => {
     // CHAR(ch, if(ch==c) 1 * mark else 0 * mark )
    val newMark = if (ch == c) 1 * mark else 0 * mark
    CHAR(ch, newMark :: marked) 
    }
    case ALT(r1, r2) => ALT(shift(mark,r1,c),shift(mark,r2,c))
    case SEQ(r1,r2) =>
      if (mark == 1 && nullable(r1)==1)
      {
      //println("if case")
        ALT(SEQ(shift(mark, r1, c), shift(fin(r1), r2, c)), shift(1, r2, c))
      }
      else{
      //println("else case")
        SEQ(shift(mark, r1, c), shift(fin(r1), r2, c))   
      }  
    //  SEQ (shift(mark,r1,c), shift(mark * nullable(r1) + fin(r1),r2,c))
    case STAR(r) => STAR(shift(mark + fin(r), r,c))
    case NTIMES(r, n,nmark,counter) => // maybe shift false if n==0 or counter == n?
      if (n==0){
        ONE
      }else{
        if(counter == n){
           NTIMES(r, n,nmark,counter) // maybe we need to shift?
        } else 
            if (mark ==1 || fin(r)==1)
            { 
              val m=mark + fin(r)
              NTIMES(shift(m, r, c), n , m::nmark,counter+1)
            }
            else NTIMES(shift(0, r, c), n,0::nmark,counter)
      }
        
        
        
       
      /*
      if (n == 0) NTIMES(r, n,nmark,counter) else 
                    if (mark ==1 || fin(r)==1)
                    { val m=mark + fin(r)
                      NTIMES(shift(m, r, c), n - 1 , m::nmark,counter+1)
                    }
                    else NTIMES(shift(0, r, c), n,0::nmark,counter)
        */
    case INIT(r) => shift(1, r, c)
     
}

def nullable(r: Rexp) : Int = r match {
  case ZERO => 0
  case ONE => 1
  case CHAR(_,_) => 0
  case ALT(r1, r2) => nullable(r1) + nullable(r2)
  case SEQ(r1, r2) => nullable(r1) * nullable(r2)
  case STAR(_) => 1
  case NTIMES(r, n ,nmark , counter ) => if (n == 0) 1 else nullable(r) //?
  case INIT(r) => nullable(r)
}

def fin(r: Rexp) : Int = r match {
  case ZERO => 0
  case ONE => 0
  case CHAR(c,marked) => marked.head
  case ALT(r1, r2) => fin(r1) + fin(r2)
  case SEQ(r1, r2) => (fin(r1) * nullable(r2)) + fin(r2) 
  case STAR(r) => fin(r)
  case NTIMES(r,n,nmark , counter) => // we check the first n marks in nmarks, and if they each more than one and not more than n
    if(n == counter ) fin(r) else 0 
   // if(nmark >= n) fin(r) else 0 // 
}

def mat(r: Rexp, s: List[Char]) : Rexp = s match {
  case Nil => r
  case c::cs => mat(shift(0, r, c), cs)
}

def matcher(r: Rexp, s: List[Char]) : Int =
  if (s == Nil) nullable(r) else fin(mat(r, s))


def matcher2(r: Rexp, s: List[Char]) : Rexp =
  val x=mat(r, s)
  x match {
    case INIT(r) => 
      //println(s"Final Reg=$r")
      r
    case _ => 
     // println(s"Final Reg=$x")
      x
  }
  
enum VALUE {
  case ZEROV
  case EMPTY
  case CHARV(c: Char)
  case UNMARKED(s:String)
  case SEQV(v1: VALUE, r2: VALUE )
  case LEFT(v: VALUE)
  case RIGHT(v: VALUE)
  case STARV(vs: List[VALUE])
}
import VALUE._

def mkeps_marked(r: Rexp): VALUE = r match {
    case ONE => EMPTY
    case CHAR(c,marked) => if(marked.head >=1) CHARV(c)
      else UNMARKED(c.toString())
    case ALT(r1, r2) =>
      if (fin(r1) == 1) LEFT(mkeps_marked(r1))
      else if (fin(r2) == 1) RIGHT(mkeps_marked(r2))
      else UNMARKED("ALT")
    case SEQ(r1, r2) => SEQV(mkeps_marked(r1), mkeps_marked(r2))
    case STAR(r1) =>
      if (fin(r1) == 1) STARV(List(mkeps_marked(r1)))
      else STARV(Nil)
    case ZERO => ZEROV
  }

  //construct function which should generate a marked regex indicating 
  //the marked regular expression at the point of each input charachter (stage)
  //it should produce a list of r

def extractStage(r: Rexp, stage: Int): Rexp = r match {
  case ZERO => ZERO
  case ONE  => ONE
  case CHAR(c, marks) =>
    val m = marks(stage)  //if (marks.size > stage) marks(stage) else 0
    CHAR(c, List(m))
  case ALT(r1, r2) =>
    ALT(extractStage(r1, stage), extractStage(r2, stage))
  case SEQ(r1, r2) =>
    SEQ(extractStage(r1, stage), extractStage(r2, stage))
  case STAR(r1) =>
    STAR(extractStage(r1, stage))
  case NTIMES(r, n, nmark,counter) =>
  val m = nmark(stage) // if (nmark.size > stage) nmark(stage) else 0
    NTIMES(extractStage(r, stage), n, List(m),counter)
  case INIT(r) =>
    INIT(extractStage(r, stage))
}

def extractStages(finalReg: Rexp, input: List[Char]): List[(Char, Rexp)] = {
 // (0 to input.length-1).toList.map(stage => extractStage(finalReg, stage))
  val n = input.length
  (0 until n).toList.reverse.map { stage =>
    // For stage i, the corresponding input char is at position n - i - 1.
   (input(n - stage - 1),(extractStage(finalReg, stage) ) )
  }
}


def intern2(r: Rexp) : Rexp = INIT(r)



def test00() = {
  
  val a=CHAR('a')
  val b=CHAR('b')
  //val rexp=ALT( SEQ(a, ALT(ONE, b)) , SEQ(a , b))
  val rexp=STAR(   ALT(a ,ALT(a ,b))   )

  val ss="ab".toList
  //val result=matcher(intern2(rexp),ss)
 // println("Testing mkeps \n")
 // println(s" rexp=$rexp Result=$result\n")

  val finReg=matcher2(intern2(rexp),ss)
 // println(mkeps_marked(finReg))

  // test extract
  val regsList=extractStages(finReg,ss)
  
  for ((ch,regex) <- regsList) {
  println(s"\nInput Character: $ch")
  println(s"Regex: $regex \n")
}

/*
  println("=====Test====")
  val br1 = intern2(rexp)
  println(s"ss is $ss \n rexp= $br1 \n ")

  for (i <- ss.indices) {
  println(s"${i + 1}- =shift ${ss(i)}=")
  val sPart = ss.take(i + 1)
  println(pp(mat(br1, sPart)))
  }
  println(matcher(br1,ss))
*/
}

def test01() = {
    val n=2
    val rexp=intern2(EVIL1(n))

    val ss="aaab".toList
    println(s"input is $ss \n")

    println(s"NTIMES :\n")
    println(s"\n Original NTIMES\n ${pp(rexp)}")

    val result=matcher(rexp,ss)
    val finReg=matcher2(rexp,ss)

    println(s"\n Final NTIMES\n ${pp(finReg)}")
    println(s"\n NTIMES Result=$result and Size=${size(finReg)} \n \n")

   // val regsList=extractStages(finReg,ss)

    //for ((ch,regex) <- regsList) {
    //  println(s"\nInput Character: $ch")
    //  println(s"Regex: $regex \n")
   // }

    println("\n========================\n")

    val a=CHAR('a')
    val opta=ALT(a,ONE)

    val testRexp=intern2(SeqNTIMES(EVIL1(n)))
    println(s"input is $ss \n")

    println(s"Original SEQ :\n")
    println(s"\n ${pp(testRexp)}")

    val resultSeq=matcher(testRexp,ss)
    val finReg2=matcher2(testRexp,ss)

    println(s"\n Final SEQ: \n ${pp(finReg2)}")
    println(s"\n SEQ rexp :Result=$resultSeq Size=${size(finReg2)} \n\n")
/*
    val regsList=extractStages(finReg2,ss)

    for ((ch,regex) <- regsList) {
      println(s"\nInput Character: $ch")
      println(s"Regex: $regex \n")
      println(pp(regex))
    }
*/

}



object Main {
  def main(args: Array[String]): Unit = {
    test03() // calls your @main function
  }
}

def test02() = {

  val a=CHAR('a')
  val b=CHAR('b')
  val opta=ALT(a,ONE)
  val optb=ALT(b,ONE)
  val newS="a".toList

  //SEQ(NTIMES(OPT(CHAR('a')), n), NTIMES(CHAR('a'), n))
  val newRexp=SEQ(opta,opta) 
  val newRexpI= intern2(newRexp)
  println(s"newS is $newS \n rexp= $newRexpI \n ")
  for (i <- newS.indices) {
  println(s"${i + 1}- =shift ${newS(i)}=")
  val sPart = newS.take(i + 1)
  println(pp(mat(newRexpI, sPart)))
  }
  println(matcher(newRexpI,newS))


  println("\n========================\n")

  val sequence=SEQ(SEQ(opta,ONE),a)
   
  val sequenceT=intern2(sequence)
  val finReg3=matcher2(intern2(sequenceT),"a".toList)
  println(pp(finReg3))
  println(s"The result is :${fin(finReg3)}")

}

def test03() = {
    val n=2
    val rexp=EVIL1(n)
    println("=====Test====")
    val br1 = intern2(rexp)
    val s = "aabbbbbbbbbbb".toList
    println(s"s is $s \n rexp= $br1 \n ")

    for (i <- s.indices) {
    println(s"${i + 1}- =shift ${s(i)}=")
    val sPart = s.take(i + 1)
    println(pp(mat(br1, sPart)))
    }
    println(matcher(br1,s))
}

def size(r: Rexp) : Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_,_) => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r) => 1 + size(r)
  case NTIMES(r,n, nmark,counter) => 1 + size(r) 
  case INIT(r) => 1 + size(r)   
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
  case NTIMES(r, n, nmark,counter) =>
    val expandR = SeqNTIMES(r)
    if (n <= 0) ONE else
        if (n == 1) expandR else 
            { SeqNTIMES( SEQ( expandR, NTIMES(expandR,n-1,nmark,counter) ) ) }
  case INIT(r) => ZERO
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

def test1() = {
  for (i <- 0 to 11000 by 1000) {
    println(f"$i: ${time_needed(2, matcher(EVIL1(i), ("a" * i).toList))}%.5f")
  }
}

//@arg(doc = "Test (a*)* b")

def test2() = {
  for (i <- 0 to 7000000 by 500000) {
    println(f"$i: ${time_needed(2, matcher(EVIL2, ("a" * i).toList))}%.5f")
  }
} 

//@arg(doc = "All tests.")

def all() = { test1(); test2() } 

// pretty-printing REGs
def implode(ss: Seq[String]) = ss.mkString("\n")
def explode(s: String) = s.split("\n").toList

def lst(s: String) : String = explode(s) match {
  case hd :: tl => implode(" └" ++ hd :: tl.map("  " ++ _))
  case Nil => ""
}

def mid(s: String) : String = explode(s) match {
  case hd :: tl => implode(" ├" ++ hd :: tl.map(" │" ++ _))
  case Nil => ""
}

def indent(ss: Seq[String]) : String = ss match {
  case init :+ last => implode(init.map(mid) :+ lst(last))
  case _ => "" 
}

def pp(e: Rexp) : String = e match {
  case ZERO => "0\n"
  case ONE => "1\n"
  case CHAR(c,marked) => if (marked.head>=1) s"•$c m:$marked\n" else s"$c m:$marked\n"
  case ALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => "SEQ\n" ++ pps(r1, r2)
  case STAR(r) => "STAR\n" ++ pps(r) //if(n == 0 ) fin(r) 
  case NTIMES(r,n,nmark,counter) => if (n == 0 && fin(r) == 1) s"• NTIMES(n=$n) $nmark counter=$counter\n" ++ pps(r) 
    else s"NTIMES(n=$n) $nmark counter=$counter\n" ++ pps(r)
  case INIT(r) => "INIT\n" ++ pps(r)
}
def pps(es: Rexp*) = indent(es.map(pp))


/*
def matcher(r: Rexp , s: String) : Int = s.toList match {
  case Nil => nullable(r)
//case c :: cs => fin(cs.foldLeft(shift(true, r, c)) { (acc, c) =>
//  shift(false, acc, c) })
  case c :: cs => //fin(cs.foldLeft(shift(1, r, c))(shift(0, _, _)))
    val x=cs.foldLeft(shift(1, r, c))(shift(0, _, _))
    println(s"\n Last Reg: \n $x")
    fin(x)
}
*/ 
}

```



#### Error stacktrace:

```
scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:8)
	dotty.tools.dotc.core.Denotations$SingleDenotation.updateValidity(Denotations.scala:718)
	dotty.tools.dotc.core.Denotations$SingleDenotation.bringForward(Denotations.scala:743)
	dotty.tools.dotc.core.Denotations$SingleDenotation.toNewRun$1(Denotations.scala:800)
	dotty.tools.dotc.core.Denotations$SingleDenotation.current(Denotations.scala:871)
	dotty.tools.dotc.core.Symbols$Symbol.recomputeDenot(Symbols.scala:117)
	dotty.tools.dotc.core.Symbols$Symbol.computeDenot(Symbols.scala:111)
	dotty.tools.dotc.core.Symbols$Symbol.denot(Symbols.scala:104)
	dotty.tools.dotc.core.Symbols$ClassSymbol.classDenot(Symbols.scala:491)
	dotty.tools.dotc.core.Symbols$.toClassDenot(Symbols.scala:507)
	dotty.tools.dotc.core.TypeErasure$.normalizeClass(TypeErasure.scala:108)
	dotty.tools.dotc.core.TypeErasure.dotty$tools$dotc$core$TypeErasure$$sigName(TypeErasure.scala:909)
	dotty.tools.dotc.core.TypeErasure$.sigName(TypeErasure.scala:234)
	dotty.tools.dotc.core.Signature$.apply(Signature.scala:167)
	dotty.tools.dotc.core.Types$MethodOrPoly.computeSignature$2(Types.scala:3776)
	dotty.tools.dotc.core.Types$MethodOrPoly.signature(Types.scala:3796)
	dotty.tools.dotc.core.Denotations$SingleDenotation.signature(Denotations.scala:618)
	dotty.tools.dotc.core.Denotations$SingleDenotation.matchesLoosely(Denotations.scala:1026)
	dotty.tools.dotc.core.Denotations$SingleDenotation.matches(Denotations.scala:1010)
	dotty.tools.dotc.core.Denotations$Denotation.mergeDenot$1(Denotations.scala:410)
	dotty.tools.dotc.core.Denotations$Denotation.meet(Denotations.scala:500)
	dotty.tools.dotc.core.Denotations$DenotUnion.toDenot(Denotations.scala:1232)
	dotty.tools.dotc.core.Denotations$DenotUnion.toDenot(Denotations.scala:1232)
	dotty.tools.dotc.core.SymDenotations$ClassDenotation.findMember(SymDenotations.scala:2170)
	dotty.tools.dotc.core.Types$Type.go$1(Types.scala:737)
	dotty.tools.dotc.core.Types$Type.findMember(Types.scala:916)
	dotty.tools.dotc.core.Types$Type.memberBasedOnFlags(Types.scala:720)
	dotty.tools.dotc.core.Types$Type.member(Types.scala:704)
	dotty.tools.dotc.typer.ProtoTypes$SelectionProto.isMatchedBy(ProtoTypes.scala:207)
	dotty.tools.dotc.core.TypeComparer.isMatchedByProto(TypeComparer.scala:2101)
	dotty.tools.dotc.core.TypeComparer.firstTry$1(TypeComparer.scala:337)
	dotty.tools.dotc.core.TypeComparer.recur(TypeComparer.scala:1472)
	dotty.tools.dotc.core.TypeComparer.isSubType(TypeComparer.scala:206)
	dotty.tools.dotc.core.TypeComparer.isSubType(TypeComparer.scala:216)
	dotty.tools.dotc.core.TypeComparer.topLevelSubType(TypeComparer.scala:126)
	dotty.tools.dotc.core.TypeComparer.testSubType(TypeComparer.scala:142)
	dotty.tools.dotc.core.TypeComparer$.testSubType(TypeComparer.scala:3011)
	dotty.tools.dotc.typer.Typer.adaptNoArgsOther$1(Typer.scala:4084)
	dotty.tools.dotc.typer.Typer.adaptNoArgs$1(Typer.scala:4166)
	dotty.tools.dotc.typer.Typer.adapt1(Typer.scala:4393)
	dotty.tools.dotc.typer.Typer.adapt(Typer.scala:3669)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3382)
	dotty.tools.dotc.typer.Typer.typeSelectOnTerm$1(Typer.scala:809)
	dotty.tools.dotc.typer.Typer.typedSelect(Typer.scala:847)
	dotty.tools.dotc.typer.Typer.tryApply$1(Typer.scala:3469)
	dotty.tools.dotc.typer.Typer.tryInsertApplyOrImplicit$$anonfun$2(Typer.scala:3500)
	dotty.tools.dotc.typer.Typer.tryEither(Typer.scala:3406)
	dotty.tools.dotc.typer.Typer.tryInsertApplyOrImplicit(Typer.scala:3507)
	dotty.tools.dotc.typer.Typer.adaptToArgs$1(Typer.scala:3788)
	dotty.tools.dotc.typer.Typer.adapt1(Typer.scala:4388)
	dotty.tools.dotc.typer.Typer.adapt(Typer.scala:3669)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3382)
	dotty.tools.dotc.typer.Applications.realApply$1(Applications.scala:969)
	dotty.tools.dotc.typer.Applications.typedApply(Applications.scala:1160)
	dotty.tools.dotc.typer.Applications.typedApply$(Applications.scala:380)
	dotty.tools.dotc.typer.Typer.typedApply(Typer.scala:116)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3116)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3382)
	dotty.tools.dotc.typer.Namer.typedAheadExpr$$anonfun$1(Namer.scala:1701)
	dotty.tools.dotc.typer.Namer.typedAhead(Namer.scala:1691)
	dotty.tools.dotc.typer.Namer.typedAheadExpr(Namer.scala:1701)
	dotty.tools.dotc.typer.Namer.typedAheadRhs$1$$anonfun$1(Namer.scala:1955)
	dotty.tools.dotc.inlines.PrepareInlineable$.dropInlineIfError(PrepareInlineable.scala:242)
	dotty.tools.dotc.typer.Namer.typedAheadRhs$1(Namer.scala:1955)
	dotty.tools.dotc.typer.Namer.rhsType$1(Namer.scala:1963)
	dotty.tools.dotc.typer.Namer.cookedRhsType$1(Namer.scala:1981)
	dotty.tools.dotc.typer.Namer.lhsType$1(Namer.scala:1982)
	dotty.tools.dotc.typer.Namer.inferredResultType(Namer.scala:1993)
	dotty.tools.dotc.typer.Namer.inferredType$1(Namer.scala:1739)
	dotty.tools.dotc.typer.Namer.valOrDefDefSig(Namer.scala:1746)
	dotty.tools.dotc.typer.Namer$Completer.typeSig(Namer.scala:801)
	dotty.tools.dotc.typer.Namer$Completer.completeInCreationContext(Namer.scala:952)
	dotty.tools.dotc.typer.Namer$Completer.complete(Namer.scala:828)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.completeFrom(SymDenotations.scala:174)
	dotty.tools.dotc.core.Denotations$Denotation.completeInfo$1(Denotations.scala:188)
	dotty.tools.dotc.core.Denotations$Denotation.info(Denotations.scala:190)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.ensureCompleted(SymDenotations.scala:392)
	dotty.tools.dotc.typer.Typer.retrieveSym(Typer.scala:3057)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3082)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3196)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3293)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3339)
	dotty.tools.dotc.typer.Typer.typedBlockStats(Typer.scala:1225)
	dotty.tools.dotc.typer.Typer.typedBlock(Typer.scala:1229)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3124)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3382)
	dotty.tools.dotc.typer.Typer.caseRest$1(Typer.scala:1941)
	dotty.tools.dotc.typer.Typer.typedCase(Typer.scala:1957)
	dotty.tools.dotc.typer.Typer.typedCases$$anonfun$1(Typer.scala:1885)
	dotty.tools.dotc.core.Decorators$.loop$1(Decorators.scala:94)
	dotty.tools.dotc.core.Decorators$.mapconserve(Decorators.scala:110)
	dotty.tools.dotc.typer.Typer.typedCases(Typer.scala:1884)
	dotty.tools.dotc.typer.Typer.$anonfun$34(Typer.scala:1877)
	dotty.tools.dotc.typer.Applications.harmonic(Applications.scala:2418)
	dotty.tools.dotc.typer.Applications.harmonic$(Applications.scala:380)
	dotty.tools.dotc.typer.Typer.harmonic(Typer.scala:116)
	dotty.tools.dotc.typer.Typer.typedMatchFinish(Typer.scala:1877)
	dotty.tools.dotc.typer.Typer.typedMatch(Typer.scala:1811)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3130)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3382)
	dotty.tools.dotc.typer.Typer.$anonfun$57(Typer.scala:2553)
	dotty.tools.dotc.inlines.PrepareInlineable$.dropInlineIfError(PrepareInlineable.scala:242)
	dotty.tools.dotc.typer.Typer.typedDefDef(Typer.scala:2553)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3092)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3196)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3293)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3339)
	dotty.tools.dotc.typer.Typer.typedClassDef(Typer.scala:2736)
	dotty.tools.dotc.typer.Typer.typedTypeOrClassDef$1(Typer.scala:3104)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3108)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3196)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3293)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3339)
	dotty.tools.dotc.typer.Typer.typedPackageDef(Typer.scala:2879)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3149)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3320)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3339)
	dotty.tools.dotc.typer.Typer.typedPackageDef(Typer.scala:2879)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3149)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3197)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3267)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3271)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3382)
	dotty.tools.dotc.typer.TyperPhase.typeCheck$$anonfun$1(TyperPhase.scala:45)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	dotty.tools.dotc.core.Phases$Phase.monitor(Phases.scala:458)
	dotty.tools.dotc.typer.TyperPhase.typeCheck(TyperPhase.scala:51)
	dotty.tools.dotc.typer.TyperPhase.$anonfun$4(TyperPhase.scala:97)
	scala.collection.Iterator$$anon$6.hasNext(Iterator.scala:479)
	scala.collection.Iterator$$anon$9.hasNext(Iterator.scala:583)
	scala.collection.immutable.List.prependedAll(List.scala:152)
	scala.collection.immutable.List$.from(List.scala:685)
	scala.collection.immutable.List$.from(List.scala:682)
	scala.collection.IterableOps$WithFilter.map(Iterable.scala:900)
	dotty.tools.dotc.typer.TyperPhase.runOn(TyperPhase.scala:96)
	dotty.tools.dotc.Run.runPhases$1$$anonfun$1(Run.scala:315)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.ArrayOps$.foreach$extension(ArrayOps.scala:1323)
	dotty.tools.dotc.Run.runPhases$1(Run.scala:308)
	dotty.tools.dotc.Run.compileUnits$$anonfun$1(Run.scala:349)
	dotty.tools.dotc.Run.compileUnits$$anonfun$adapted$1(Run.scala:358)
	dotty.tools.dotc.util.Stats$.maybeMonitored(Stats.scala:69)
	dotty.tools.dotc.Run.compileUnits(Run.scala:358)
	dotty.tools.dotc.Run.compileSources(Run.scala:261)
	dotty.tools.dotc.interactive.InteractiveDriver.run(InteractiveDriver.scala:161)
	dotty.tools.pc.MetalsDriver.run(MetalsDriver.scala:45)
	dotty.tools.pc.WithCompilationUnit.<init>(WithCompilationUnit.scala:31)
	dotty.tools.pc.SimpleCollector.<init>(PcCollector.scala:345)
	dotty.tools.pc.PcSemanticTokensProvider$Collector$.<init>(PcSemanticTokensProvider.scala:63)
	dotty.tools.pc.PcSemanticTokensProvider.Collector$lzyINIT1(PcSemanticTokensProvider.scala:63)
	dotty.tools.pc.PcSemanticTokensProvider.Collector(PcSemanticTokensProvider.scala:63)
	dotty.tools.pc.PcSemanticTokensProvider.provide(PcSemanticTokensProvider.scala:88)
	dotty.tools.pc.ScalaPresentationCompiler.semanticTokens$$anonfun$1(ScalaPresentationCompiler.scala:109)
```
#### Short summary: 

java.lang.AssertionError: assertion failed: denotation class Int invalid in run 3. ValidFor: Period(4.1-2)