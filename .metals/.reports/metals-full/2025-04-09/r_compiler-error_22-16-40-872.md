file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/play_explicit_bits_2.sc
### java.lang.NullPointerException: Cannot invoke "scala.meta.internal.pc.CompilerWrapper.compiler()" because "access" is null

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/play_explicit_bits_2.sc
text:
```scala
import scala.language.implicitConversions
import $file.rexp, rexp._, rexp.Rexp._, rexp.VALUE._
import $file.derivativesBitcode, derivativesBitcode._

enum BRexp {
  case BZERO 
  case BONE(bs: List[Int]=List()) 
  case BCHAR(c: Char,bs: List[Int]=List()) 
  case BALT(r1: BRexp, r2: BRexp) 
  case BSEQ(r1: BRexp, r2: BRexp) 
  case BSTAR(r: BRexp) 
  case BNTIMES(r: BRexp, n: Int , counter: Int=0)
  case NOT(r: BRexp) 
  case BPOINT(bs: List[Int], r: BRexp) // might not need the bs here
  case BINIT(r: BRexp)
}

import BRexp._







// nullable 
def nullable(r: Rexp) : Boolean = (r: @unchecked) match {
  case ZERO => false
  case ONE => true
  case CHAR(d) =>  false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(r) => true
  case POINT(_, r) => nullable(r)
}

def mkeps(r: Rexp) : Bits = (r: @unchecked) match {
  case ONE => Nil
  case POINT(bs, CHAR(_)) => bs
  case ALT(r1, r2) => 
    if (nullable(r1)) Z :: mkeps(r1) else S :: mkeps(r2)  
  case SEQ(r1, r2) => mkeps(r1) ++ mkeps(r2)
  case STAR(r) => mkeps(r) ++ List(S)
  case CHAR(_) => Nil //for testing mkeps outside lex
}
// fin function from the paper
// checks whether a mark is in "final" position
def fin(r: Rexp) : Boolean = (r: @unchecked) match {
  case ZERO => false
  case ONE => false
  case CHAR(_) => false
  case POINT(_, CHAR(_)) => true
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
}

//maybe add count matches?


def finSize(r: Rexp, nullable:Boolean) : Int = r match {
  case ZERO => 0
  case ONE =>  if(nullable) 1 else 0
  case CHAR(_) => 0
  case POINT(bs, r) =>  1 
  case ALT(r1, r2) => 1 + finSize(r1,nullable) + finSize(r2,nullable)
  case SEQ(r1, r2) => 1 + finSize(r1,nullable) + finSize(r2,nullable)
  case STAR(r) => 1 + finSize(r,nullable)
}


def mkfin(r: Rexp) : Bits = (r: @unchecked) match {
  case POINT(bs, CHAR(_)) => bs
  case ALT(r1, r2) => if (fin(r1)) mkfin(r1) else mkfin(r2)  
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin(r1) ++ mkeps(r2)
  case SEQ(r1, r2) => mkfin(r2)
  case STAR(r) => mkfin(r) ++ List(S)
}


// shift function from the paper
def shift(m: Boolean, bs: Bits, r: Rexp, c: Char) : Rexp = (r: @unchecked) match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if (m && d == c) POINT(bs, CHAR(d)) else CHAR(d)
  case POINT(_, CHAR(d)) => if (m && d == c) POINT(bs, CHAR(d)) else CHAR(d)
  case ALT(r1, r2) => ALT(shift(m, bs :+ Z, r1, c), shift(m, bs :+ S, r2, c))
  case SEQ(r1, r2) if m && nullable(r1) => SEQ(shift(m, bs, r1, c), shift(true, bs ::: mkeps(r1), r2, c))
  case SEQ(r1, r2) if fin(r1) => SEQ(shift(m, bs, r1, c), shift(true, mkfin(r1), r2, c))
  case SEQ(r1, r2) => SEQ(shift(m, bs, r1, c), shift(false, Nil, r2, c))
  case STAR(r) if m && fin(r) => STAR(shift(true, bs ::: (mkfin(r) :+ Z), r, c))
  case STAR(r) if fin(r) => STAR(shift(true, mkfin(r) :+ Z, r, c)) 
  case STAR(r) if m => STAR(shift(m, bs, r, c))
  case STAR(r) => STAR(shift(false, Nil, r, c))
}
// the main matching function (by using BINIT only in 
// the first step a mark is shifted into the Rexp)
def mat(r: BRexp, s: List[Char]) : BRexp = s match {
  case Nil => r
  case c::cs => mat(shift(false, r, c,List()), cs)
}

def matcher(r: Rexp, s: List[Char]) : Boolean =
    val reg= intern(r)
    if (s == Nil) nullable(reg)
     else fin(mat(reg, s))

def matcher2(r: Rexp, s: List[Char]) : Rexp =
    val reg= intern2(r)
    if (s == Nil) if(nullable(reg)) reg else MZERO 
    else mat(reg, s)

def lex(r: Rexp, s: List[Char]) : Option[List[Int]] = {
  va
  if matcher(r, s)
  then Some(if (s == Nil) mkeps(r) else mkfin(mat(r, s)))
  else None
}

def intern(r: Rexp) : BRexp = (r: @unchecked) match{
  case ZERO            => BZERO
  case ONE             => BONE
  case CHAR(c)         => BCHAR(c, Mark()) 
  case ALT(r1, r2)     => BALT(intern(r1), intern(r2))
  case SEQ(r1, r2)     => BSEQ(intern(r1), intern(r2))
  case STAR(r)         => BSTAR(intern(r))
  case NTIMES(r, n)    => BNTIMES(intern(r), n, 0)
  //case INIT(r)         => BINIT(intern(r)) // might remove
}

def intern2(r: Rexp) : BRexp = BINIT(intern(r))

// testing one/emptystring regex 
@main
def test1() = {
  println("=====Test With ONE====")
  val rexp=SEQ(ALT(ONE,CHAR('c')) , ALT(SEQ(CHAR('c'),CHAR('c')), CHAR('c')) )
  val s = "cc".toList

  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(rexp, sPart)))
  } 

  val finReg=matcher2(rexp,s)
  val bits=bitsToInts(lex(rexp, s).getOrElse(Nil))
  println(s"\n=final list= ${bits}\n")

  println(s"\nFinal Reg:= ${finReg}\n")
  println(s"mkfin: ${mkfin(finReg)}")
  println(s"\nDecoded value for Marked=${decode( bits, rexp)._1}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  println(s"\nDerivatives bitcode: $derivBitcode")
  println(s"\nDecoded value for derivatives=${decode( derivBitcode, rexp)._1}")
  
}

//testing seq,alt,char only regex
@main
def test2() = {
  println("=====Test With SEQ/ALT/CHAR only====")
  val rexp=SEQ(
    ALT(ALT(CHAR('a'),CHAR('b')),SEQ(CHAR('a'),CHAR('b'))) , 
    ALT( SEQ(CHAR('b'),CHAR('c')), ALT(CHAR('c'),CHAR('b'))) ) 
  val s = "abc".toList

  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(rexp, sPart)))
  } 

  val finReg=matcher2(rexp,s)
  val bits=bitsToInts(lex(rexp, s).getOrElse(Nil))
  println(s"\n=final list= ${bits}\n")

  println(s"\nFinal Reg:= ${finReg}\n")
  println(s"mkfin: ${mkfin(finReg)}")
  println(s"\nDecoded value for Marked=${decode( bits, rexp)._1}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  println(s"\nDerivatives bitcode: $derivBitcode")
  println(s"\nDecoded value for derivatives=${decode( derivBitcode, rexp)._1}")
}

@main
def test3() = {
  println("=====Test With SEQ/ALT/CHAR only====")
  val rexp=("a" | "ab") ~ ("b" | ONE)
  val s = "ab".toList
  
  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(rexp, sPart)))
  } 

  val finReg=matcher2(rexp,s)
  val bits=bitsToInts(lex(rexp, s).getOrElse(Nil))
  println(s"\n=final list= ${bits}\n")

  println(s"\nFinal Reg:= ${finReg}\n")
  println(s"mkfin: ${mkfin(finReg)}")
  println(s"\nDecoded value for Marked=${decode( bits, rexp)._1}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  println(s"\nDerivatives bitcode: $derivBitcode")
  println(s"\nDecoded value for derivatives=${decode( derivBitcode, rexp)._1}")
  
}


```



#### Error stacktrace:

```
dotty.tools.pc.ScalaPresentationCompiler.semanticdbTextDocument$$anonfun$1(ScalaPresentationCompiler.scala:240)
```
#### Short summary: 

java.lang.NullPointerException: Cannot invoke "scala.meta.internal.pc.CompilerWrapper.compiler()" because "access" is null