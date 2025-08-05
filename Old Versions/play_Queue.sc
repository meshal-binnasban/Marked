//
// Algorithm from "A Play on Regular Expressions"
//
// augmented with bitsequences



import scala.language.implicitConversions


abstract class Bit
/*
case object Z extends Bit {
  override def toString = "0"
}
case object S extends Bit {
  override def toString = "1"
}
*/
// values
abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(v1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val
case class Nt(vs: List[Val], n: Int) extends Val // new to testX1.
//case object XXX extends Val


// regular expressions
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 
case class NTIMES(r: Rexp,n:Int) extends Rexp // new to testX1.
// only used by re-generate to generate non-matching strings
case class NOT(r: Rexp) extends Rexp 
case class POINT(bs: List[Bits], r: Rexp) extends Rexp
case class APPOINT(mkl: Mark, r: Rexp) extends Rexp // active/passive point

//List[Bits] for file:play_explicit_List.sc

def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

// strings are coerced into Rexps
given Conversion[String, Rexp] = (s => charlist2rexp(s.toList))

//val HELLO : Rexp = "hello"

extension (r: Rexp) {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}


def nullable(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, n) => n == 0 || nullable(r) 
  case POINT(_, r) => nullable(r)
  case APPOINT(mk, r) => nullable(r) // active/passive point
}

// Bits of NTIMES
case object NxT extends Bit {
  override def toString = "Nt"
}
case object EnT extends Bit {
  override def toString = "Et"
}
// End of Bits of NTIMES

case object St extends Bit {
  override def toString = "("
}
case object Cl extends Bit {
  override def toString = ")"
}

case object Sq2 extends Bit {
  override def toString = "Se"
}
// Bits of the SEQ
case object Sq extends Bit {
  override def toString = "S"
}
// End of Bits of the SEQ

// Bits of Character Consumption
case object Ch extends Bit {
  override def toString = "C"
}
case object Ep extends Bit {
  override def toString = "Ep"
}
// End of Bits of Character Consumption

//original Bits
case object Lf extends Bit {
  override def toString = "L"
}
case object Ri extends Bit {
  override def toString = "R"
}
case object Nx extends Bit {
  override def toString = "N"
}
case object En extends Bit {
  override def toString = "E"
}

type Bits = List[Bit]

case class Mark(
  mark: Boolean,
  active: Boolean,
  bits: List[Bits],
  str: List[Char],
  finSeq: Boolean = false
) {
  override def toString: String = {
    val bitsStr = bits.map(_.mkString(",")).mkString("/")
    val strStr = str.mkString
    s"(mark=$mark, active=$active, bits=$bitsStr, remaining str='$strStr', finSeq= $finSeq )"
  }
}

abstract class PRexp
case object PZERO extends PRexp
case object PONE extends PRexp
case class PCHAR(c: Char) extends PRexp
case class PALT(r1: PRexp, r2: PRexp) extends PRexp
case class PSEQ(r1: PRexp, r2: PRexp,mlist:List[Mark]) extends PRexp 
case class PPOINT(mk: Mark, r: PRexp) extends PRexp 

def pnullable(r: PRexp) : Boolean = r match {
  case PZERO => false
  case PONE => true
  case PCHAR(_) => false
  case PALT(r1, r2) => pnullable(r1) || pnullable(r2)
  case PSEQ(r1, r2,mlist) => pnullable(r1) && pnullable(r2)
  case PPOINT(mk, r) => pnullable(r)
}

extension (xs: List[Bits]) {
  def <*> (ys: List[Bits]): List[Bits] =
    for (x <- xs; y <- ys) yield x ::: y

  def <+> (y: Bit): List[Bits] =
    for (x <- xs) yield x :+ y

  def <++>(ys: Bits): List[Bits] =
    xs.map(_ ++ ys) 

  def <::>(y: Bit): List[Bits] = 
    for (x <- xs) yield y :: x
}


def pmkeps(r: PRexp) : Bits = r match {
  case PONE => List(Ep)
  case PALT(r1, r2) =>
    if (pnullable(r1)) Lf :: pmkeps(r1) else Ri :: pmkeps(r2)
  case PSEQ(r1, r2,mlist) =>
    pmkeps(r1) ++ pmkeps(r2)
}

def pfin(r: PRexp) : Boolean = (r: @unchecked) match 
  case PZERO => false
  case PONE => false
  //case APPOINT(mk, ONE) => if(mk.str.isEmpty) true else false
  case PCHAR(_) => false
  case PPOINT(_, PCHAR(_)) => true 
  case PALT(r1, r2) => pfin(r1) || pfin(r2)
  case PSEQ(r1, r2,mlist) => (pfin(r1) && pnullable(r2)) || pfin(r2)


def pmkfin(r: PRexp): List[Bits] = r match 
  case PPOINT(mark, PCHAR(_)) => mark.bits
  case PALT(r1, r2) if pfin(r1) && pfin(r2) => pmkfin(r1) ::: pmkfin(r2)
  case PALT(r1, r2) if pfin(r1) => pmkfin(r1)
  case PALT(r1, r2) if pfin(r2) => pmkfin(r2)
  case PSEQ(r1, r2,mlist) if pfin(r1) && pnullable(r2) && pfin(r2) => pmkfin(r2) ::: pmkfin(r1).map(_ ++ (pmkeps(r2))) 
  case PSEQ(r1, r2,mlist) if pfin(r1) && pnullable(r2) => pmkfin(r1).map(_ ::: (pmkeps(r2))) 
  case PSEQ(r1, r2,mlist) => pmkfin(r2)


def pmkfin2(r: PRexp): List[Bits] = r match 
  case PPOINT(mark, PCHAR(_)) =>mark.bits
  case PALT(r1, r2) if pfin(r1)  => pmkfin2(r1) 
  case PALT(r1, r2)  => pmkfin2(r2)
  case PSEQ(r1, r2,mlist) if pfin(r1) && pnullable(r2)  => pmkfin2(r1).map(_ ++ (pmkeps(r2))) 
  case PSEQ(r1, r2,mlist) => pmkfin2(r2)

def pshift(mk: Mark, r: PRexp): (PRexp,List[Mark]) = r match 
    case PZERO => (PZERO,List())
    case PONE => (PONE,List())
    case PCHAR(d) => 
        if (mk.mark && mk.str.head == d) 
            val newMk=  mk.copy( bits = mk.bits <+> Ch,str=mk.str.tail)
            (PPOINT(newMk, PCHAR(d)),List(newMk))
        else (PCHAR(d),List())
    case PPOINT(m, PCHAR(d)) => 
        if (mk.mark && mk.str.head == d) //active= if(mk.str.tail.isEmpty) false else mk.active,
            val newMk=mk.copy(bits = mk.bits <+> Ch,str=mk.str.tail )
            (PPOINT(newMk,PCHAR(d)),List(newMk))
        else (PCHAR(d),List())
    case PALT(r1, r2) =>
        val shiftedR1=pshift(mk.copy(bits = mk.bits <+> Lf), r1)
        val shiftedR2=pshift(mk.copy(bits = mk.bits <+> Ri), r2)
        (PALT(shiftedR1._1, shiftedR2._1) , shiftedR1._2 ++ shiftedR2._2)

    case PSEQ(r1, r2,mlist) if pfin(r1) =>
        val shiftedR1=pshift(mk, r1)

        val mk2=mk.copy(bits = pmkfin(r1), active=false)
        val mlist2=sortMarks(mk2::mlist)

        val poped=mlist2.head.copy(mark=true)
        val shiftedR2=pshift(poped, r2)
        (PSEQ(shiftedR1._1, shiftedR2._1, mlist2.tail), shiftedR1._2++shiftedR2._2) //poped::mlist2.tail
        
    case PSEQ(r1, r2,mlist) if pnullable(r1) =>
        val shiftedR1=pshift(mk, r1)

        val mk2=mk.copy(bits = mk.bits <++> pmkeps(r1),active = false)
        val mlist2=sortMarks(mk2::mlist)

        val poped=mlist2.head.copy(mark=true)
        val shiftedR2=pshift(poped, r2)
        (PSEQ(shiftedR1._1, shiftedR2._1, mlist2.tail),shiftedR1._2++shiftedR2._2)//poped::mlist2.tail

    case PSEQ(r1, r2,mlist) =>
        val shiftedR1=pshift(mk, r1)
        val shiftedR2=pshift(mk.copy(mark = false,active=false, bits = Nil), r2)
        (PSEQ(shiftedR1._1, shiftedR2._1,mlist),mlist)

def pmat(r: PRexp, s: List[Char],prnt:Boolean=false): PRexp =
  val initMk = Mark(mark = true, active = true, bits = List(List()), str = s)
  val initR=pshift(initMk, r)
  val marksRemaining=sortMarks(initR._2)
  println(s"Initial Regex: \n${pp(initR._1)}\n marksRemaining: ${marksRemaining}\n") 

  if(marksRemaining.nonEmpty){
    val tes=pshift(marksRemaining.head,initR._1)
    println(s"\n\ntes Regex= \n${pp(tes._1)} \n tes marks: ${(tes._2)}\n\n\n")
    
    }
  initR._1 
    

def sortMarks(marks: List[Mark]): List[Mark] = {
  marks.sortBy(mk => (!mk.active, mk.str.length))
  //.filter(_.str.nonEmpty)
}


def pmatcher(r: PRexp, s: List[Char]) : Boolean =
  if (s == Nil) pnullable(r) else pfin(pmat(r, s,true))

def plex(r: PRexp, s: List[Char]): Option[List[Bits]] =
  if pmatcher(r, s)
  then Some(if (s == Nil) List(pmkeps(r)) else pmkfin2(pmat(r, s)))
  else None

def intern(r:Rexp): PRexp = r match {
  case ZERO => PZERO
  case ONE => PONE
  case CHAR(c) => PCHAR(c)
  case ALT(r1, r2) => PALT(intern(r1), intern(r2))
  case SEQ(r1, r2) => PSEQ(intern(r1), intern(r2), List())
}

//(ONE | "a" ) ~ ( "a" | "aa" )
@main 
def test1() = {
  println("=====Test====")
  //val br2 = (ONE | "a" ) ~ ( "a" | "aa" )
  val br2= intern( (ONE | "a") ~ ("a" | "aa"))
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")
  println(s"=mat=\n${pmat(br2, s,true)}\n")
}

//("a" | "ab") ~ ("c" | "bc")
@main
def test2() = {
  println("=====Test====")
  val br2= intern(  ("a" | "ab") ~ ("c" | "bc") )
    //%("aa") | ("aa" ~ ONE)
  val s = "abc".toList
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")
  println(s"=mat=\n${pmat(br2, s,true)}\n")
}

//(("a"|"b") | ("ab")) ~ (("bc") | ("c" | "b"))
@main
def test3() = {
  println("=====Test====")
  val br2=intern( (("a"|"b") | ("ab")) ~ (("bc") | ("c" | "b")) )
    //%("aa") | ("aa" ~ ONE)
  val s = "abc".toList
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")
  println(s"=mat=\n${pmat(br2, s,true)}\n")
}

//( ONE | ( ONE | "bc" )  )  | ( "a"| ONE ) ~ ("a" | "aa")
@main
def test4() = {
  println("=====Test====")
  val br2= intern (( ONE | ( ONE | "bc" )  )  | ( "a"| ONE ) ~ ("a" | "aa") )
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")
  println(s"=mat=\n${pmat(br2, s,true)}\n")
}

@main
def test5() = {
  println("=====Test====")
  val br2= intern ( ("aa" | "bb") )
  val s = "bb".toList
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")
  println(s"=mat=\n${pmat(br2, s,true)}\n")
}

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

def pp(e: PRexp) : String = (e: @unchecked) match { 
  case PZERO => "0\n"
  case PONE => s"1 \n"
  case PCHAR(c) => s"$c\n"
  case PPOINT(mark, PCHAR(c)) => s"•$c:${mark}\n" 
  case PALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case PSEQ(r1, r2,mlist) => s"SEQ Marks: ${mlist.mkString(",")} \n" ++ pps(r1, r2)
  
}

def pps(es: PRexp*) = indent(es.map(pp))

def flatten(v: Val) : String = v match {
   case Empty => ""
   case Chr(c) => c.toString
   case Left(v) => flatten(v)
   case Right(v) => flatten(v)
   case Sequ(v1, v2) => flatten(v1) ++ flatten(v2)
   case Stars(vs) => vs.map(flatten).mkString
   case Nt(vs, _) => vs.map(flatten).mkString
   //case Rec(_, v) => flatten(v)
 }

