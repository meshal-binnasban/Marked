import scala.language.implicitConversions
import $file.rexp, rexp._, rexp.Rexp._, rexp.VALUE._
import $file.derivativesBitcode, derivativesBitcode._

enum BRexp {
  case BZERO 
  case BONE(bs: List[Int]=List()) 
  case BCHAR(c: Char, bs: List[Int]=List()) 
  case BALT(r1: BRexp, r2: BRexp, bs: List[Int]=List()) 
  case BSEQ(r1: BRexp, r2: BRexp, bs: List[Int]=List()) 
  case BSTAR(r: BRexp, bs: List[Int]=List()) 
  case BNTIMES(r: BRexp, n: Int , counter: Int=0 , bs: List[Int]=List())
  case NOT(r: BRexp) 
  case BPOINT(r: BRexp, bs: List[Int]) // might not need the bs here
  case BINIT(r: BRexp)
}

import BRexp._
// nullable 
def nullable(r: BRexp) : Boolean = (r: @unchecked) match {
  case BZERO => false
  case BONE(bs) => true
  case BCHAR(d,bs) =>  false
  case BALT(r1, r2,bs) => nullable(r1) || nullable(r2)
  case BSEQ(r1, r2,bs) => nullable(r1) && nullable(r2)
  case BSTAR(r,bs) => true
  case BPOINT(r,bs) => nullable(r)
  case BINIT(r) => nullable(r) // added to check nullable input
}



// fin function from the paper
// checks whether a mark is in "final" position
def fin(r: BRexp) : Boolean = (r: @unchecked) match {
  case BZERO => false
  case BONE(_) => false
  case BCHAR(_,_) => false
  case BPOINT(BCHAR(_,_),bs) => true
  case BALT(r1, r2, bs) => fin(r1) || fin(r2)
  case BSEQ(r1, r2, bs) => (fin(r1) && nullable(r2)) || fin(r2)
  case BSTAR(r, bs) => fin(r)
  case BINIT(r) => fin(r) // added to check nullable input
}


def mkeps(r: BRexp) : List[Int] = (r: @unchecked) match {
  case BONE(bs) =>  // flag added to indicate a regex used empty string to match
    println(s"here, ONE, bs=$bs")
    8::bs
  case BPOINT(BCHAR(c,bs), pbs) => pbs
  case BALT(r1, r2, bs) => 
    if (nullable(r1)) 0 :: mkeps(r1) else 1 :: mkeps(r2)  
  case BSEQ(r1, r2, bs) => mkeps(r1) ++ mkeps(r2) // if nullable r1 add 2?
  case BSTAR(r, bs) => mkeps(r) ++ List(1)
  case BCHAR(_,_) => Nil //for testing mkeps outside lex
  case BINIT(r) => mkeps(r) // added to check nullable input
}

def mkfin(r: BRexp) : List[Int] = (r: @unchecked) match {
  //case BONE(bs) => if(nullable) bs else Nil, nullable:Boolean
  //if nullable r1 then add mkeps?
  case BONE(bs) => bs
  case BPOINT(BCHAR(_,bs), pbs) => pbs
  case BALT(r1, r2, bs) => if (fin(r1)) mkfin(r1) else mkfin(r2)  
  case BSEQ(r1, r2, bs) if fin(r1) && nullable(r2) => mkfin(r1) ++ mkeps(r2)
  case BSEQ(r1, r2, bs) => mkfin(r2)
  case BSTAR(r, bs) => mkfin(r) ++ List(1)
  case BINIT(r) => mkfin(r)
}

// shift function from the paper
def shift(m: Boolean, bs: List[Int], r: BRexp, c: Char) : BRexp = (r: @unchecked) match {
  case BZERO => BZERO
  case BONE(bcs) => BONE(bs)
  case BCHAR(d,bcs) => if (m && d == c) BPOINT(BCHAR(d,Nil),bs:+7) else BCHAR(d,Nil)
  case BPOINT(BCHAR(d,bcs), pbs) => if (m && d == c) BPOINT(BCHAR(d, Nil), bs:+7) else BCHAR(d,Nil)
  case BALT(r1, r2, bcs) => BALT(shift(m, bs:+0 , r1, c), shift(m, bs:+1, r2, c), Nil)

  case BSEQ(r1, r2, bcs) if m && nullable(r1) => BSEQ(shift(m, bs, r1, c), shift(true, (bs ::: mkeps(r1) ), r2, c) , Nil) //:::List(6)
  case BSEQ(r1, r2, bcs) if fin(r1) => BSEQ(shift(m, bs, r1, c), shift(true, mkfin(r1), r2, c) , Nil)
  case BSEQ(r1, r2, bcs) => BSEQ(shift(m, bs, r1, c), shift(false, Nil, r2, c) , Nil)

  case BSTAR(r, bcs) if m && fin(r) => BSTAR(shift(true, bs ::: (mkfin(r)), r, c), Nil)
  case BSTAR(r, bcs) if fin(r) => BSTAR(shift(true, mkfin(r), r, c),  Nil) 
  case BSTAR(r, bcs) if m => BSTAR(shift(m, bs, r, c) , Nil)
  case BSTAR(r, bcs) => BSTAR(shift(false, Nil, r, c),Nil)
  case BINIT(r) => shift(true, bs, r, c)
}
// the main matching function (by using BINIT only in 
// the first step a mark is shifted into the Rexp)
def mat(r: BRexp, s: List[Char]) : BRexp = s match {
  case Nil => r
  case c::cs => mat(shift(false,List(), r, c), cs)
}

def matcher(r: Rexp, s: List[Char]) : Boolean =
    val reg= intern2(r)
    if (s == Nil) nullable(reg)
     else fin(mat(reg, s))

def matcher2(r: Rexp, s: List[Char]) : BRexp =
    val reg= intern2(r)
    if (s == Nil) if(nullable(reg)) reg else BZERO 
    else mat(reg, s)

def lex(r: Rexp, s: List[Char]) : Option[List[Int]] = {
  val reg=intern2(r)
  if matcher(r, s)
  then Some(if (s == Nil) mkeps(reg) else mkfin(mat(reg, s)))
  else None
}

def intern(r: Rexp) : BRexp = (r: @unchecked) match{
  case ZERO            => BZERO
  case ONE             => BONE(List())
  case CHAR(c)         => BCHAR(c, List()) 
  case ALT(r1, r2)     => BALT(intern(r1), intern(r2), List())
  case SEQ(r1, r2)     => BSEQ(intern(r1), intern(r2), List())
  case STAR(r)         => BSTAR(intern(r), List())
  case NTIMES(r, n)    => BNTIMES(intern(r), n, 0 , List())
  //case INIT(r)         => BINIT(intern(r)) // might remove
}

def intern2(r: Rexp) : BRexp = BINIT(intern(r))

def mDecode(bs: List[Int], r: Rexp): (VALUE, List[Int]) = 
    val cleanedBs = bs.filterNot(_ == 2)
    (r: @unchecked) match {
    case ONE => (EMPTY, bs)
    case CHAR(c) => (CHARV(c), bs)
    case ALT(r1, r2) => bs match {
        case 0 :: rest => val (v, rem) = decode(rest, r1); (LEFT(v), rem)
        case 1 :: rest => val (v, rem) = decode(rest, r2); (RIGHT(v), rem)
        case _ => (ERRORVALUE, bs)
    }
    case SEQ(r1, r2) =>
        val (v1, bs1) = decode(bs, r1)
        val (v2, bs2) = decode(bs1, r2)
        (SEQV(v1, v2), bs2)
    case STAR(r) => bs match {
        case 1 :: rest => (STARV(List()), rest)
        case 0 :: rest => 
        val (v, bs1) = decode(rest, r)
        val (STARV(vs), bs2) = decode(bs1, STAR(r))
        (STARV(v :: vs), bs2)
        case _ => (ERRORVALUE, bs)
    }
}
// testing one/emptystring regex 
@main
def test1() = {
  println("=====Test With ONE====")
  val rexp=SEQ( ALT(ONE, CHAR('c')) ,ALT(SEQ(CHAR('c'),CHAR('c')), CHAR('c')) )
  val brexp=intern2(rexp)
  val s = "cc".toList

  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(brexp, sPart)))
  } 

  val finReg=matcher2(rexp,s)
  val bits=lex(rexp, s).getOrElse(Nil)
  println(s"\n=final list= ${bits}\n")

  println(s"\nFinal Reg:= ${finReg}\n\n ${pp(finReg)} nullable=${nullable(finReg)}")
  println(s"mkfin: ${mkfin(finReg)}")
  println(s"\nDecoded value for Marked=${mDecode( bits, rexp)._1}")

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
  val brexp=intern2(rexp)
  val s = "abc".toList

  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(brexp, sPart)))
  } 

  val finReg=matcher2(rexp,s)
  val bits=lex(rexp, s).getOrElse(Nil)
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
  val brexp=intern2(rexp)
  val s = "ab".toList
  
  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(brexp, sPart)))
  } 

  val finReg=matcher2(rexp,s)
  val bits=lex(rexp, s).getOrElse(Nil)
  println(s"\n=final list= ${bits}\n")

  println(s"\nFinal Reg:= ${finReg}\n")
  println(s"mkfin: ${mkfin(finReg)}")
  println(s"\nDecoded value for Marked=${decode( bits, rexp)._1}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  println(s"\nDerivatives bitcode: $derivBitcode")
  println(s"\nDecoded value for derivatives=${decode( derivBitcode, rexp)._1}")
  
}

def pp(e: BRexp) : String = (e: @unchecked) match {
  case BZERO => "0\n"
  case BONE(bs) => s"1 ${if(bs.length>0) s":${bs.mkString(",")}" else ""}\n"
  case BCHAR(d,bs) => s"$d ${if(bs.length>0) s":${bs.mkString(",")}" else ""}\n"
  case BPOINT(BCHAR(c,bs), pbs) => s"${"\u001b[32m"} •$c :${pbs.mkString(",")} ${"\u001b[0m"}\n" 
  case BALT(r1, r2, bs) => s"ALT ${if(bs.length>0) s":${bs.mkString(",")}" else ""}\n" ++ pps(r1, r2)
  case BSEQ(r1, r2, bs) => s"SEQ ${if(bs.length>0) s":${bs.mkString(",")}" else ""}\n" ++ pps(r1, r2)
  case BSTAR(r, bs) => s"STAR ${if(bs.length>0) s":${bs.mkString(",")}" else ""}\n" ++ pps(r)
  case BINIT(r) => s"INIT\n" ++ pps(r)
}
def pps(es: BRexp*) = indent(es.map(pp))


/* 
def finSize(r: BRexp, nullable:Boolean) : Int = (r: @unchecked) match {
  case BZERO => 0
  case BONE(bs) =>  if(nullable) 1 else 0
  case BCHAR(_,_) => 0
  case BPOINT(r, bs) =>  1 
  case BALT(r1, r2, bs) => 1 + finSize(r1,nullable) + finSize(r2,nullable)
  case BSEQ(r1, r2 , bs) => 1 + finSize(r1,nullable) + finSize(r2,nullable)
  case BSTAR(r, bs) => 1 + finSize(r,nullable)
} */

/*     if(fin(r1) && fin(r2)){
      var mkfinr1=mkfin(r1)
      var mkfinr2=mkfin(r2)

      if(nullable(r1))
      mkfinr1++mkeps(r1)

      if(nullable(r2))
      mkfinr2++mkeps(r2)

        println(s"mkfin r1= ${mkfinr1} and mkfin r2 =${mkfinr2}")
        println(s"nullable r1= ${nullable(r1)} and nullable r2 =${nullable(r2)}")
        if(mkfinr1.length < mkfinr2.length){
          mkfinr1
        }else
          mkfinr2
      }    
    else 
        if (fin(r1)){
        println(s"mkfin fin1 final & nullable=${nullable(r1)}")
        if(nullable(r1)) mkfin(r1)++mkeps(r1) else mkfin(r1) 
        }
        else{
        println(s"mkfin fin2 final & nullable=${nullable(r2)}")
        if(nullable(r2)) mkfin(r2)++mkeps(r2) else mkfin(r2) 
        }  */