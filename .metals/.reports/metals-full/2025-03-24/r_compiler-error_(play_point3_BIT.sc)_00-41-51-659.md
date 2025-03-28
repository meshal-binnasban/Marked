file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/.ammonite/scala-2.13.14/amm-3.0.0-2-6342755f/play_point3_BIT/src/ammonite/$file/play_point3_BIT.amm.sc.scala
### dotty.tools.dotc.core.TypeError$$anon$1: Toplevel definition mid is defined in
  <WORKSPACE>/play_point3_BIT.sc
and also in
  <WORKSPACE>/.ammonite/scala-2.13.14/amm-3.0.0-2-6342755f/play_point3_BIT/src/ammonite/$file/play_point3_BIT.amm.sc.scala
One of these files should be removed from the classpath.

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 6844
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/.ammonite/scala-2.13.14/amm-3.0.0-2-6342755f/play_point3_BIT/src/ammonite/$file/play_point3_BIT.amm.sc.scala
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


object play_point3_BIT{
/*<start>*/
import scala.language.implicitConversions
import os.size

enum Rexp {
  case ZERO 
  case ONE 
  case CHAR(c: Char )
  case POINT(r: Rexp)
  case ALT(r1: Rexp, r2: Rexp) 
  case SEQ(r1: Rexp, r2: Rexp) 
  case STAR(r: Rexp) 
  case NTIMES(r: Rexp, n: Int , counter: Int = 0)
  case BIT(r:Rexp , bs:List[Int]=List())
}
import Rexp._

def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case POINT(r) => nullable(r)
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, n,counter) => if (n == 0) true else nullable(r)
}

def fin(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => false
  case CHAR(_) => false 
  case POINT(CHAR(_)) => true 
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
  case NTIMES(r, n,counter) => counter == n && fin(r)
  case POINT(r) => fin(r) //?
}

//shift char with position
def shift(m: Boolean, re: Rexp, c: Char) : Rexp = {
  re match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) =>
    if(m && d == c) POINT(CHAR(d)) 
    else CHAR(d)
  case POINT(CHAR(d)) => 
    if(m && d == c) re
    else CHAR(d)
  case ALT(r1, r2) => 
    ALT(shift(m, r1, c), shift(m, r2, c)) 
  case SEQ(r1, r2) => 
    SEQ(shift(m, r1, c), shift((m && nullable(r1)) || fin(r1), r2, c))
  case STAR(r) => 
    STAR(shift(m || fin(r), r, c)) // bs ++ mkeps(r) ++ List(0) drops the performance in case of evil regex
  case NTIMES(r, n,counter) => 
    if (counter == n) re
      else{
        if (m || fin(r)) NTIMES(shift(m || fin(r), r, c), n, counter+1)
        else NTIMES(shift(false, r, c), n, counter)       
        } // if shifted r is final, wrap in point? didn't work
} //end match r
}

def mat(r: Rexp, s: List[Char]) : Rexp = s match {
  case Nil => r
  case c::cs => cs.foldLeft(shift(true, r, c))((r, c) => shift(false, r, c))
}

def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s))

def matcher2(r: Rexp, s: List[Char]) : Rexp =
  if (s == Nil)
     if(nullable(r)) r else ZERO 
     else mat(r, s)

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

    println("\n=============== Final Reg ===============\n")
    println(s"Size=${size(finReg)} , Tree= \n ${pp(finReg)}\n")
    println("\n=============== bitcodes ===============\n")

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

enum VALUE {
  case ZEROV
  case ONEV
  case CHARV(c: Char)
  case UNMARKED(s:String)
  case SEQV(v1: VALUE, r2: VALUE )
  case LEFT(v: VALUE)
  case RIGHT(v: VALUE)
  case STARV(vs: List[VALUE])
}
import VALUE._

def decode(r: Rexp, bs: List[Int]): (VALUE, List[Int]) = r match {
  case ONE => (ONEV, bs) // not sure this should be included
  case CHAR(c) => (CHARV(c), List(1)) // (2) decode (c) bs = (Char(c), bs)
  case ALT(r1, r2) => bs match {
    case 0 :: bs1 => // (3) decode (r1 + r2) 0 :: bs =  (Left(v), bs')  where decode r1 bs => v,bs' 
        val (v, bsp) = decode(r1, bs1)
        (LEFT(v), bsp) 
    case 1 :: bs1 => // (4) decode (r1 + r2) 1 :: bs = (Right(v), bs') where decode r2 bs => v,bs'
        val (v, bsp) = decode(r2, bs1)
        (RIGHT(v), bsp)
    case x =>
      (ZEROV, bs) // in case of something else, may need to remove it but just incase
  }
  case SEQ(r1, r2) =>  // (5) decode (r1 · r2) bs = (Seq(v1, v2), bs3) where decode r1 bs => v1,bs2 and decode r2 bs2 =>v2,bs3 
    val (v1, bs2) = decode(r1, bs)
    val (v2, bs3) = decode(r2, bs2)
    (SEQV(v1, v2), bs3) 

  case STAR(r) => bs match {
    case 1 :: bs1 => 
      (STARV(List()), bs1) // terminate recursion for STAR
    case 0 :: bs1 =>   
      val (v, bs2) = decode(r, bs1)
      val (STARV(vs), bsv) = decode(STAR(r), bs2) 
      (STARV(v :: vs), bsv) 
    case _ => 
      (STARV(List()), bs) // Edge case: No matches in STAR
    }// end of match r
}

// exctract the bits from the bit constructor, maybe also fuse it into the empty normal regular expression?
def mkeps(r: Rexp): List[Int] = r match {
    case ONE => List() 
    case CHAR(_) => List() 
    case POINT(CHAR(c)) => List(1)
    case ALT(r1, r2) =>
        if (fin(r1))  List(0) ++ mkeps(r1) 
        else if (fin(r2)) List(1) ++ mkeps(r2) 
        else List() 
    case SEQ(r1, r2) =>
      if (fin(r1) && nullable(r2)) mkeps(r1) ++ mkeps(r2) 
      else if (fin(r2)) mkeps(r2)
      else List() //bs
    case STAR(r) =>
        //r match case point add zero else 1 ? also tag?
        if (fin(r)) List(1) // ++mkeps_marked2(r)++
        else List() 
    case NTIMES(r, n, counter) =>
        if (counter == n && fin(r)) mkeps(r) 
        else List() //bs
    case ZERO => List() 
}

def fuse(cs: List[Int], r: Rexp): Rexp =BIT(r,cs++bs)
  
    /* 
    {
    case ZERO => ZERO
    case ONE => ONE
    case CHAR(c) => CHAR(c)//cs ++ List(0)
    case POINT(r) => POINT(r)
    case ALT(r1, r2) => ALT(r1, r2)
    case SEQ(r1, r2) => SEQ(r1, r2)
    case STAR(r) => STAR(r)
    case NTIMES(r, n, c@@ounter) => NTIMES(fuse(cs, r), n, counter) */



// maybe here create the bit object, updated by a new function called attach bits?
def intern(r: Rexp) : Rexp = BIT(r)

/*
  r match{
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(c) => CHAR(c)
  case ALT(r1, r2) => 
    ALT(fuse(List(0), intern(r1)), fuse(List(1), intern(r2)))
  case SEQ(r1, r2) => SEQ(intern(r1),intern(r2))
  case STAR(r) => STAR(intern(r))
  case NTIMES(r, n,counter) => NTIMES(intern(r),n,counter)
  } */


def size(r: Rexp) : Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_) => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r) => 1 + size(r)
  case NTIMES(r,n,counter) => 1 + size(r) 
  case POINT(r) => 1 + size(r)}

// some syntax sugar for regexes
def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
// strings are coerced into Rexps
given Conversion[String, Rexp] = s => charlist2rexp(s.toList)

//val ABCD : Rexp = "abcd"

extension (r: Rexp) {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

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
  case CHAR(c) => s"$c\n"
  case POINT(CHAR(c)) => s"•$c \n" 
  case ALT(r1, r2) => s"ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => s"SEQ\n" ++ pps(r1, r2)
  case STAR(r) => "STAR\n" ++ pps(r)
  case NTIMES(r, n,counter) => 
    s"NTIMES{$n} {counter=$counter}\n" ++ pps(r)
  case POINT(r) =>pp(r)
}
def pps(es: Rexp*) = indent(es.map(pp))

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
} 
}

```



#### Error stacktrace:

```

```
#### Short summary: 

dotty.tools.dotc.core.TypeError$$anon$1: Toplevel definition mid is defined in
  <WORKSPACE>/play_point3_BIT.sc
and also in
  <WORKSPACE>/.ammonite/scala-2.13.14/amm-3.0.0-2-6342755f/play_point3_BIT/src/ammonite/$file/play_point3_BIT.amm.sc.scala
One of these files should be removed from the classpath.