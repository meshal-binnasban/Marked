error id: _empty_/MALT.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/play_explicit_bits_seq.sc
empty definition using pc, found symbol in pc: 
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -rexp/MALT.
	 -rexp/MALT#
	 -rexp/MALT().
	 -enumerate/MALT.
	 -enumerate/MALT#
	 -enumerate/MALT().
	 -scala/util/MALT.
	 -scala/util/MALT#
	 -scala/util/MALT().
	 -MALT.
	 -MALT#
	 -MALT().
	 -scala/Predef.MALT.
	 -scala/Predef.MALT#
	 -scala/Predef.MALT().
offset: 2618
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


/* def decode_aux(r: MRexp, bs: Bits) : (Val, Bits) = (r, bs) match {
  case (MONE(_), bs) => (Empty, bs)
  case (MCHAR(_,c), bs) => (Chr(c), bs)
  case (MALT(cs,r1, r2), Lf::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    (Left(v), bs1)
  }
  case (MALT(cs,r1, r2), Ri::bs) => {
    val (v, bs1) = decode_aux(r2, bs)
    (Right(v), bs1)
  }
  case (MSEQ(cs,r1, r2), bs) => {
    val (v1, bs1) = decode_aux(r1, bs)
    val (v2, bs2) = decode_aux(r2, bs1)
    (Sequ(v1, v2), bs2)
  }
  case (MSTAR(_,_), En::bs) => (Stars(Nil), bs)
  case (MSTAR(cs,r1), Nx::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    val (Stars(vs), bs2) = (decode_aux(MSTAR(cs,r1), bs1)  : @unchecked)
    (Stars(v::vs), bs2)
  }
}

def dec2(r: MRexp, bs: Bits) = decode_aux(r, bs) match {
  case (v, Nil) => v
  case _ => throw new Exception("Not decodable")
} */

def mnullable (r: MRexp) : Boolean = r match {
  case MZERO => false
  case MONE(_) => true
  case MCHAR(_,_) => false
  case MALT(_, r1, r2) => mnullable(r1) || mnullable(r2)
  case MSEQ(_, r1, r2) => mnullable(r1) && mnullable(r2)
  case MSTAR(_, _) => true
  case MNTIMES(cs, r, n) => n == 0 || mnullable(r) // new to testX1.
  case MPOINT(_, r) => mnullable(r)
}

def mkeps(r: MRexp) : Bits = r match {
  case MONE(cs) => cs
  case MPOINT(bs, MONE(cs)) => bs
  case MPOINT(bs, MCHAR(_,_)) => bs
  case MALT(bs,r1, r2) => 
    if (mnullable(r1)) Lf :: mkeps(r1) else Ri :: mkeps(r2) //??? 
  case MSEQ(bs, r1, r2) => mkeps(r1) ++ mkeps(r2)
  case MSTAR(bs ,r) => List(En)
  //case NTIMES(r, n) if nullable(r)=> mkeps(r) ++ List(En) // new to testX1.
  case MNTIMES(bs ,r, n) => List(En) // new to testX1.
}

def fuse(bs: Bits, r: MRexp) : MRexp = r match {
  case MZERO => MZERO
  case MONE(cs) => MONE(bs ++ cs)
  case MCHAR(cs ,c) => MCHAR(bs ++ cs,c)
  case MPOINT(cs, MCHAR(css,c)) => MPOINT(bs ++ cs, MCHAR(css,c))
 // case MPOINT(cs, r) => MPOINT(bs ++ cs, r)
  case M@@ALT(cs, r1, r2) => MALT(bs ++ cs, r1, r2)
  case MSEQ(cs, r1, r2) => MSEQ(bs ++ cs, r1, r2)
  case MSTAR(cs, r) => MSTAR(bs ++ cs, r)
  case MNTIMES(cs, r,n) => MNTIMES(bs ++ cs, r,n)
}

def intern(r: Rexp) : MRexp = r match {
  case ZERO => MZERO
  case ONE => MONE(Nil)
  case CHAR(c) => MCHAR(Nil,c)
  case ALT(r1, r2) => MALT(Nil,  intern(r1), intern(r2))
  case SEQ(r1, r2) => MSEQ(Nil, intern(r1), intern(r2))
  case STAR(r) => MSTAR(Nil, intern(r))
  case NTIMES(r, n) => MNTIMES(Nil, intern(r),n) 
}

def fin(r: MRexp) : Boolean = (r: @unchecked) match {
  case MZERO => false
  case MONE(_) => false
  case MCHAR(_,_) => false
  case MPOINT(_, MCHAR(_,_)) => true
  case MALT(bs,r1, r2) => fin(r1) || fin(r2)
  case MSEQ(bs,r1, r2) => (fin(r1) && mnullable(r2)) || fin(r2)
  case MSTAR(bs, r) => fin(r)
  case MNTIMES(bs, r, n) => if(n==0) fin(r) else false // new to testX1.
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
  case MNTIMES(bs, r, n) => mkfin2(r).map(_ ++ List(En)) 
}

// shift function from the paper
def shift(m: Boolean, bs: Bits, r: MRexp, c: Char) : MRexp = (r: @unchecked) match {
  case MZERO => MZERO
  case MONE(cs) => MONE(cs)
  case MCHAR(cs ,d) => if (m && d == c) MPOINT(cs, MCHAR(Nil,d)) else MCHAR(Nil,d)
  case MPOINT(_, MCHAR(cs,d)) => if (m && d == c) MPOINT(cs, MCHAR(Nil,d)) else MCHAR(Nil,d)
  
  case MPOINT(bss, MONE(cs)) =>  MPOINT(bss,MONE(cs))
  
  case MALT(cs ,r1, r2) => MALT(Nil,shift(m, Nil, fuse(cs:+Lf, r1), c), shift(m, Nil, fuse(cs:+Ri, r2), c))

  case MSEQ(cs,r1, r2) if m && mnullable(r1) => 
    MALT(Nil,MSEQ(Nil,shift(true, Nil, fuse(cs,r1), c), shift(fin(r), Nil , fuse(cs:::mkeps(r1),r2), c)) 
    , shift(true,Nil, fuse(cs:::mkeps(r1),r2), c) )

  case MSEQ(cs,r1, r2) if fin(r1) => MSEQ(Nil,shift(m, Nil, fuse(cs,r1), c), shift(true, Nil, fuse(mkfin(r1),r2), c))
  case MSEQ(cs,r1, r2) => MSEQ(Nil,shift(m, Nil, fuse(cs,r1), c), shift(false, Nil, r2, c))

  case MSTAR(cs,r) if m => MSEQ(Nil ,shift(true, Nil, fuse(cs :+ Nx,r), c), MSTAR(Nil,r))
  case MSTAR(cs,r)=> MSTAR(Nil,shift(false, Nil, r, c))

  case MNTIMES(cs ,r,n) if n <=0 =>MPOINT(List(En), MONE(Nil))
  case MNTIMES(cs ,r,n) if m => MSEQ(Nil ,shift(true, Nil, fuse(cs:+Nx,r), c) , MNTIMES(List(En) ,r,n-1))
  case MNTIMES(cs ,r,n) =>MNTIMES(Nil,shift(false, Nil, r, c),n)
}

// the main matching function (by using BINIT only in 
// the first step a mark is shifted into the Rexp)
def mat(r: MRexp, s: List[Char]) : MRexp = s match {
  case Nil => r
  case c::cs => cs.foldLeft(shift(true, Nil, r, c))((r, c) => shift(false, Nil, r, c))
}

def matcher(r: MRexp, s: List[Char]) : Boolean =
  if (s == Nil) mnullable(r) else fin(mat(r, s))

def lex(r: MRexp, s: List[Char]) : Option[Set[Bits]] = {
  if matcher(r, s)
  then Some(if (s == Nil) Set(mkeps(r)) else mkfin2(mat(r, s)))
  else None
}

/* def lexer(r: MRexp, s: List[Char]) : Option[Set[Val]] = {
  lex(r, s).map(_.map(dec2(r, _)))
} */

def OPT(r: Rexp) = ALT(ONE, r)

//
@main
def basic() = {
  println("=====Test====")
  val br = ONE |  NTIMES("a" | "aa",1) 
  val br2=intern(br)
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
  println(rebit.lex(br, s))
}


//%("a")~(%("ab") ~ %("b")) - Works now - after r.r* and SEQ nullable = ALT
@main
def test1() = {
  println("=====Test====")
  val br = %("a")~(%("ab") ~ %("b"))
  val br2=intern(br)
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
  println(rebit.lex(br, s))
}

//%("a") ~ ("aa"|"a") -  works now - check more input chars
@main
def test2() = {
  println("=====Test====")

  val br= %("a") ~ ("aa"|"a")
  val br2=intern(br)
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
  println(rebit.lex(br, s))
}

//(ONE  |  %("c"|"d")) - works now - check more input chars
@main
def test3() =  {
  println("=====Test====")
  val br = (ONE  |  %("c"|"d"))
  val br2=intern(br)
  val s = "ccc".toList
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
  println(rebit.lex(br, s))
}

// ONE  |  "a" ~ %("a") - Works now - after r.r* and SEQ nullable = ALT
@main
def test4() =  {
  println("=====Test====")
  val br = ONE  |  "a" ~ %("a")
  val br2=intern(br)
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
  println(rebit.lex(br, s))
}

// ONE | %("a" | "aa") - fails on aaa - after r.r* and SEQ nullable = ALT
@main
def test5() =  {
  println("=====Test====")
  val br = ONE | %("a" | "aa")
  val br2=intern(br)
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
  println(rebit.lex(br, s))
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
      { val v1s = Try(lex(intern(r), s.toList)).getOrElse(None)
        val v2 = rebit.lex(r, s.toList)
        if (v1s.isDefined && !v1s.get.contains(v2)) {
          println(s"[$i] reg: $r str: $s")
          println(s"mark: ${v1s.get} bder: $v2")
          println(s"mark: ${lex(intern(r), s.toList).get} bder: ${rebit.lex(r, s.toList)}")
          print("Continue testing? (y/n): ")
          val input = scala.io.StdIn.readLine().trim.toLowerCase
          if (input != "y") {
            println("End test.")
            System.exit(0) 
          }
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

def pp(e: MRexp) : String = (e: @unchecked) match {
  case MZERO => "0\n"
  case MONE(cs) => s"1:${cs.mkString(",")}\n"
  case MCHAR(cs,c) => s"$c\n"
  case MPOINT(cs, MCHAR(css,c)) => s"•$c:${cs.mkString(",")}\n" 
  case MPOINT(bs, MONE(cs)) => s"1:${bs.mkString(",")}\n" 
  case MALT(cs ,r1, r2) => s"ALT :${cs.mkString(",")}\n" ++ pps(r1, r2)
  case MSEQ(cs,r1, r2) => s"SEQ:${cs.mkString(",")}\n" ++ pps(r1, r2)
  case MSTAR(cs ,r) => s"STAR:${cs.mkString(",")}\n" ++ pps(r)
  case MNTIMES(cs,r, n) => s"NTIMES($n):${cs.mkString(",")}\n" ++ pps(r)
}
def pps(es: MRexp*) = indent(es.map(pp))



// case MSTAR(cs,r) if m && fin(r)=> MSEQ(cs,shift(true, Nil , fuse(cs:::(mkfin(r) :+ Nx),r), c), MSTAR(cs,r))
// case MSTAR(cs,r) if fin(r)=> MSEQ(cs ,shift(true, Nil , fuse((mkfin(r) :+ Nx),r), c), MSTAR(cs,r))

//case MNTIMES(cs ,r,n) if fin(r) =>MSEQ(Nil ,shift(true, Nil, fuse(mkfin(r),r), c) , MNTIMES(mkfin(r),r,n-1)) 

//MSEQ(cs,shift(m, Nil, fuse(cs,r1), c), shift(true, Nil , fuse(cs ::: mkeps(r1),r2), c))


```


#### Short summary: 

empty definition using pc, found symbol in pc: 