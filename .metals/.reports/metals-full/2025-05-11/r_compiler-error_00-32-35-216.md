file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/play_explicit_bits_seq.sc
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 4809
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/play_explicit_bits_seq.sc
text:
```scala


import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate//, regenerate._
import $file.rebit

abstract class MRexp 
case object MZERO extends MRexp
case class MONE(bs: Bits) extends MRexp
case class MCHAR(bs:Bits ,c: Char) extends MRexp
case class MPOINT(bs: Bits, r: MRexp) extends MRexp
case class MALT(bs: Bits, r1: MRexp, r2: MRexp) extends MRexp 
case class MSEQ(bs: Bits, r1: MRexp, r2: MRexp) extends MRexp 
case class MSTAR(bs: Bits, r: MRexp) extends MRexp 
case class MNTIMES(bs: Bits, r: MRexp , n:Int) extends MRexp // new to testX1.


def decode_aux(r: Rexp, bs: Bits) : (Val, Bits) = (r, bs) match {
  case (ONE, bs) => (Empty, bs)
  case (CHAR(c), bs) => (Chr(c), bs)
  case (ALT(r1, r2), Lf::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    (Left(v), bs1)
  }
  case (ALT(r1, r2), Ri::bs) => {
    val (v, bs1) = decode_aux(r2, bs)
    (Right(v), bs1)
  }
  case (SEQ(r1, r2), bs) => {
    val (v1, bs1) = decode_aux(r1, bs)
    val (v2, bs2) = decode_aux(r2, bs1)
    (Sequ(v1, v2), bs2)
  }
  case (STAR(_), En::bs) => (Stars(Nil), bs)
  case (STAR(r1), Nx::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    val (Stars(vs), bs2) = (decode_aux(STAR(r1), bs1)  : @unchecked)
    (Stars(v::vs), bs2)
  }
}

def dec2(r: Rexp, bs: Bits) = decode_aux(r, bs) match {
  case (v, Nil) => v
  case _ => throw new Exception("Not decodable")
}

def mnullable (r: MRexp) : Boolean = r match {
  case MZERO => false
  case MONE(_) => true
  case MCHAR(_,_) => false
  case MALT(_, r1, r2) => mnullable(r1) || mnullable(r2)
  case MSEQ(_, r1, r2) => mnullable(r1) && mnullable(r2)
  case MSTAR(_, _) => true
  case MNTIMES(_, _, n) => n == 0 || mnullable(r) // new to testX1.
}

def mkeps(r: MRexp) : Bits = r match {
  case MONE => Nil
  case MPOINT(bs, MCHAR(_,_)) => bs
  case MALT(bs,r1, r2) => 
    if (nullable(r1)) Lf :: mkeps(r1) else Ri :: mkeps(r2) //??? 
  case MSEQ(bs, r1, r2) => mkeps(r1) ++ mkeps(r2)
  case MSTAR(bs ,r) => List(En)
  //case NTIMES(r, n) if nullable(r)=> mkeps(r) ++ List(En) // new to testX1.
  case MNTIMES(bs ,r, n) => List(En) // new to testX1.
}

def fuse(bs: Bits, r: MRexp) : MRexp = r match {
  case MZERO => MZERO
  case MONE(cs) => MONE(bs ++ cs)
  case MCHAR(cs ,c) => MCHAR(bs ++ cs,c)
  case MPOINT(cs, MCHAR(c)) => MPOINT(bs ++ cs, c)
  case MALT(cs, r1, r2) => MALT(bs ++ cs, r1, r2)
  case MSEQ(cs, r1, r2) => MSEQ(bs ++ cs, r1, r2)
  case MSTAR(cs, r) => MSTAR(bs ++ cs, r)
  case MNTIMES(cs, r,n) => MNTIMES(bs ++ cs, r)
}

def intern(r: Rexp) : MRexp = r match {
  case ZERO => MZERO
  case ONE => MONE(Nil)
  case CHAR(c) => MCHAR(Nil,c)
  case ALT(r1, r2) => 
    MALT(Nil, fuse(List(Lf), intern(r1)), fuse(List(Ri), intern(r2)))
  case SEQ(r1, r2) => MSEQ(Nil, intern(r1), intern(r2))
  case STAR(r) => MSTAR(Nil, intern(r))
  case NTIMES(r, n) => MNTIMES(Nil, intern(r),n) 
}

def fin(r: MRexp) : Boolean = (r: @unchecked) match {
  case MZERO => false
  case MONE => false
  case MCHAR(_,_) => false
  case MPOINT(_, MCHAR(_,_)) => true
  case MALT(bs,r1, r2) => fin(r1) || fin(r2)
  case MSEQ(bs,r1, r2) => (fin(r1) && mnullable(r2)) || fin(r2)
  case MSTAR(bs, r) => fin(r)
  case MNTIMES(bs, r, n) => fin(r) 
}

def mkfin(r: MRexp) : Bits = r match {
  case MPOINT(bs, MCHAR(_,_)) => bs
  case MALT(bs ,r1, r2) => if (fin(r1)) mkfin(r1) else mkfin(r2)  
  case MSEQ(bs ,r1, r2) if fin(r1) && mnullable(r2) => mkfin(r1) ++ mkeps(r2)
  case MSEQ(bs ,r1, r2) => mkfin(r2)
  case MSTAR(bs ,r) => mkfin(r) ++ List(En)
  case MNTIMES(bs,r, n) => mkfin(r) ++ List(En) 
}

def mkfin2(r: MRexp) : Set[Bits] = r match {
  case MPOINT(bs, MCHAR(_,_)) => Set(bs)
  case MALT(bs, r1, r2) if fin(r1) && fin(r2) => mkfin2(r1) | mkfin2(r2)
  case MALT(bs ,r1, r2) if fin(r1) => mkfin2(r1)
  case MALT(bs, r1, r2) if fin(r2) => mkfin2(r2)   
  case MSEQ(bs, r1, r2) if fin(r1) && mnullable(r2) =>mkfin2(r1).map(_ ++ mkeps(r2))
  case MSEQ(bs, r1, r2) => mkfin2(r2)
  case MSTAR(bs, r) => mkfin2(r).map(_ ++ List(En))
  case MNTIMES(bs, r, n) => mkfin2(r).map(_ ++ List(En)) // new to testX1.
}

// shift function from the paper
def shift(m: Boolean, bs: Bits, r: MRexp, c: Char) : MRexp = (r: @unchecked) match {
  case MZERO => MZERO
  case MONE => MONE
  case MCHAR(cs ,d) => if (m && d == c) MPOINT(cs, MCHAR(Nil,d)) else MCHAR(cs,d)
  case POINT(_, CHAR(cs,d)) => if (m && d == c) MPOINT(cs, MCHAR(cs,d)) else MCHAR(cs,d)

  case MALT(cs ,r1, r2) => MALT(shift(m, Nil, fuse(cs:+Lf, r1), c), shift(m, Nil, fuse(cs:+Ri, r2), c))
  
  case MSEQ(cs,r1, r2) if m && nullable(r1) => 
    MSEQ(shift(m, Nil, fuse(r1,cs), c), shift(true, bs ::: mkeps(r1), fuse(@@), c))
  case SEQ(r1, r2) if fin(r1) => SEQ(shift(m, bs, r1, c), shift(true, mkfin(r1), r2, c))
  case SEQ(r1, r2) => SEQ(shift(m, bs, r1, c), shift(false, Nil, r2, c))
  
/*   case STAR(r) if fin(r) => STAR(SEQ(shift(true, bs:::(mkfin(r) :+ Nx), r, c),STAR(r )))
  case STAR(r) if m => STAR(SEQ( shift(true, bs :+ Nx, r, c) ,STAR(r)))
  case STAR(r)  => STAR(shift(false, Nil, r, c)) */

  case STAR(r) if m && fin(r)=> SEQ(shift(true, bs:::(mkfin(r) :+ Nx), r, c), STAR(r))
  case STAR(r) if fin(r)=> SEQ(shift(true, (mkfin(r) :+ Nx), r, c), STAR(r))
  case STAR(r) if m => SEQ(shift(true, bs :+ Nx, r, c), STAR(r))
  case STAR(r)=> STAR(shift(false, Nil, r, c))
  case NTIMES(r,n)=> 
    if(n==0) ONE 
    else {
      if(m && fin(r)){
        SEQ(shift(true, bs:::(mkfin(r) :+ Nx), r, c) , NTIMES(r,n-1)) // new to testX1.
      }
      else {
        if(fin(r)){
          SEQ(shift(true, mkfin(r):+Nx, r, c) , NTIMES(r,n-1)) // new to testX1.
        }
        else{
          if(m){
            SEQ(shift(true, bs:+Nx, r, c) , NTIMES(r,n-1)) // new to testX1.
          }
          else{
            NTIMES(shift(false, Nil, r, c),n)
          }
        }
      }
    }

}


// the main matching function (by using BINIT only in 
// the first step a mark is shifted into the Rexp)
def mat(r: Rexp, s: List[Char]) : Rexp = s match {
  case Nil => r
  case c::cs => cs.foldLeft(shift(true, Nil, r, c))((r, c) => shift(false, Nil, r, c))
}

def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s))

def lex(r: Rexp, s: List[Char]) : Option[Set[Bits]] = {
  if matcher(r, s)
  then Some(if (s == Nil) Set(mkeps(r)) else mkfin2(mat(r, s)))
  else None
}


def lexer(r: Rexp, s: List[Char]) : Option[Set[Val]] = {
  lex(r, s).map(_.map(dec2(r, _)))
}

def OPT(r: Rexp) = ALT(r, ONE)

@main
def test1() = {
  println("=====Test====")
  //val br2 = %("a")~(%("ab") ~ %("b")) // this one is not even matching; input ab
  val br2 = OPT(NTIMES("a",1)) ~ ( OPT(NTIMES("ab",1)) ~ OPT(NTIMES("b",1)) )// this one is not even matching; input ab
  //val br2=SEQ(STAR("a"),STAR("a"))
  //val br2=SEQ(STAR("a"),STAR("b"))
  //val br2=NTIMES("a",2)
  val s = "ab".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}

//%("a") ~ ("aa"|"a"), works if bs:+Nx only.
@main
def test2() = {
  println("=====Test====")

  val br2= %("a") ~ ("aa"|"a")
  val s = "aaaaa".toList
  println("=string=")
  println(s)
  println(s"=shift ${s(0)}=")
  println(pp(mat(br2, s.take(1))))

  println(s"=shift ${s(1)}=")
  println(pp(mat(br2, s.take(2))))

  println(s"=shift ${s(2)}=")
  println(pp(mat(br2, s.take(3))))

  println(s"=shift ${s(3)}=")
  println(pp(mat(br2, s.take(4))))

  println(s"=shift ${s(4)}=")
  println(pp(mat(br2, s.take(5))))

  println(s"=final list=")
  println(lex(br2, s.take(5)))
  println(s"=reference list=") 
  println(rebit.lex(br2, s.take(5)))
}
//(ONE  |  %("c"|"d"))
@main
def test3() = {
  println("=====Test====")
  val br2 = (ONE  |  %("c"|"d"))
  //val br2= STAR( STAR("a") )
  val s = "ccc".toList
  println("=string=")
  println(s)
  println(s"=shift ${s(0)}=")
  println(pp(mat(br2, s.take(1))))

  println(s"=shift ${s(1)}=")
  println(pp(mat(br2, s.take(2))))

  
  println(s"=shift ${s(2)}=")
  println(pp(mat(br2, s.take(3))))

  //println(s"=shift ${s(3)}=")
  //println(pp(mat(br2, s.take(4))))
  
  println(s"=final list=")
  println(lex(br2, s.take(3)))
  println(s"=reference list=") 
  println(rebit.lex(br2, s.take(3)))
}
// (ONE|"a") ~ %("a")
@main
def test4() = {
  println("=====Test====")
  //val br2 =STAR( STAR("a") )
 // val br2=SEQ(STAR(CHAR('a')),STAR(CHAR('a')))
  //val br2=SEQ(ALT(ONE,CHAR('a')),STAR(CHAR('a')))
  val br2= (ONE|"a") ~ %("a")
  val s = "aaa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)
  println(s"=shift ${s(0)}=")
  println(pp(mat(br2, s.take(1))))

  println(s"=shift ${s(1)}=")
  println(pp(mat(br2, s.take(2))))

    println(s"=shift ${s(2)}=")
  println(pp(mat(br2, s.take(3))))


  println(s"=final list=")
  println(lex(br2, s.take(3)))
  println(s"=reference list=") 
  println(rebit.lex(br2, s.take(3)))
}

// ONE |  %( "a" | "aa" )
@main
def test5() = {
  println("=====Test====")
  val br2= ONE |  %( "a" | "aa" )
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}

//Nested STAR
//ONE|%(%("a"))
@main
def test6() = {
  println("=====Test====")
  val br2= %(%("a"))
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}

@main
def test7() = {
  println("=====Test====")
  val br2= ONE | %(%("a"))
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}

@main
def test8() = {
  println("=====Test====")
  val br2= %("aa") ~ %(%("a"))
  val s = "aaa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}


// if fin r2 of seq is prefered in case of nullable r2
//( %("a") ~ %("a") ) ~ "a"
@main
def test9() = {
  println("=====Test====")
  val br2= ( %("a") ~ %("a") ) ~ "a"
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}

// caused by ALT with two final branches
//ONE | %( "a" | "aa")
@main
def test10() = {
  println("=====Test====")
  val br2= ONE | %( "a" | "aa")
  val s = "aaa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}

import scala.util._

@main
def weakTest() = {
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

  val alphabet = LazyList('a', 'b')
  for (i <- (0L to 10_000_000L)) {
    val r = enumerate.decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "")
      { val v1s = Try(lexer(r, s.toList)).getOrElse(None)
        val v2 = rebit.blexer(r, s)
        if (v1s.isDefined && !v1s.get.contains(v2)) {
          println(s"[$i] reg: $r str: $s")
          println(s"mark: ${v1s.get} bder: $v2")
          println(s"mark: ${lex(r, s.toList).get} bder: ${rebit.lex(r, s.toList)}")
          System.exit(1)
        }
      }
  }
}

//add strongTest here

// pretty-printing Rexps

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

def pp(e: Rexp) : String = (e: @unchecked) match {
  case ZERO => "0\n"
  case ONE => "1\n"
  case CHAR(c) => s"$c\n"
  case POINT(bs, CHAR(c)) => s"•$c:${bs.mkString(",")}\n" 
  case ALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => "SEQ\n" ++ pps(r1, r2)
  case STAR(r) => "STAR\n" ++ pps(r)
  case NTIMES(r, n) => s"NTIMES($n)\n" ++ pps(r)
}
def pps(es: Rexp*) = indent(es.map(pp))




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