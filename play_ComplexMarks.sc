import $file.rexp, rexp._, rexp.Rexp._, rexp.VALUE._
import $file.derivativesBitcode, derivativesBitcode._

enum Color:
  case GREEN
  case RED
  case WHITE

case class Mark(
  marked: Boolean = false,
  bs: List[Int] = List(),
  color: Color = Color.WHITE
)

enum MRexp {
  case MZERO
  case MONE
  case MCHAR(c: Char, mark: Mark)
  case MALT(r1: MRexp, r2: MRexp)
  case MSEQ(r1: MRexp, r2: MRexp)
  case MSTAR(r: MRexp)
  case MNTIMES(r: MRexp, n: Int, counter: Int = 0)
  case MINIT(r: MRexp)
}
import MRexp._

def nullable (r: MRexp) : Boolean = r match {
  case MZERO => false
  case MONE => true
  case MCHAR(_,_) => false
  case MALT(r1, r2) => nullable(r1) || nullable(r2)
  case MSEQ(r1, r2) => nullable(r1) && nullable(r2)
  case MSTAR(_) => true
  case MNTIMES(r, n,counter) => if (n == 0) true else nullable(r)
  case MINIT(r) => nullable(r)
}

def fin(r: MRexp) : Boolean = r match {
  case MZERO => false
  case MONE => false
  case MCHAR(c,mark) => mark.marked 
  case MALT(r1, r2) => fin(r1) || fin(r2)
  case MSEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case MSTAR(r) => fin(r)
  case MNTIMES(r, n,counter) => counter == n && fin(r)
  case MINIT(r) => fin(r)
}

//shift char with position
def shift(m: Boolean, re: MRexp, c: Char, bits:List[Int]) : MRexp = {
  re match {
  case MZERO => MZERO
  case MONE => MONE
  case MCHAR(d,mark) => 
    if (m && d == c)
      MCHAR(d, Mark(marked = true, bs = bits , color = Color.GREEN))
    else 
      MCHAR(d, Mark(marked = false, bs = Nil , color = Color.RED))
      //CHAR(d, Mark())
  case MALT(r1, r2) => MALT(shift(m, r1, c,bits:+0) , shift(m, r2, c,bits:+1) )

  case MSEQ(r1, r2) if m && nullable(r1) => 
    MSEQ(shift(m, r1, c, bits:+2), shift(true, r2, c ,(bits ::: mkeps(r1)):+3) )
  case MSEQ(r1, r2) if fin(r1) =>
     MSEQ(shift(m, r1, c, bits:+2), shift(true,r2, c,mkfin(r1):+3 )) 
  case MSEQ(r1, r2) => 
     MSEQ(shift(m, r1, c, bits:+2), shift(false,r2, c, Nil ))

  case MSTAR(r) if m && fin(r) => MSTAR(shift(true, r , c , bits ::: (mkfin(r) :+ 4)))
  case MSTAR(r) if fin(r) => MSTAR(shift(true,r,c ,mkfin(r) :+ 4)) 
  case MSTAR(r) if m => MSTAR(shift(m, r, c, bits))
  case MSTAR(r) => MSTAR(shift(false, r, c, Nil))

  case MNTIMES(r, n,counter) => 
    if (counter == n) 
      re 
      else{
        if (m || fin(r)) MNTIMES(shift(m || fin(r), r, c,bits), n, counter+1)
        else MNTIMES(shift(false, r, c, bits), n, counter)       
        } 

  case MINIT(r) => shift(true, r, c,bits)  
    } 
}



def mkeps(r: MRexp): List[Int] = (r: @unchecked) match {
  case MZERO => Nil
  case MONE => Nil
  case MCHAR(_, mark) => if (mark.marked) mark.bs else Nil
  case MALT(r1, r2) => if(nullable(r1)) 0 ::mkeps(r1) else 1 :: mkeps(r2)
  case MSEQ(r1, r2) => (mkeps(r1)) ::: ((mkeps(r2)))
  case MSTAR(r) => mkeps(r)++List(1)
  case MNTIMES(_, _, _) =>Nil
  case MINIT(r1) =>mkeps(r1)
}

//add boolean argument, if nullable then retrieve from one?
def mkfin(r: MRexp) : List[Int] = (r: @unchecked) match {
  case MCHAR(_, mark) => if (mark.marked) mark.bs else Nil

  case MALT(r1, r2) => 
    if(fin(r1) && fin(r2)){
      println(s"mkfin r1= ${mkfin(r1)} and mkfin r2 =${mkfin(r2)}")
    }
    
    if (fin(r1)) mkfin(r1) else mkfin(r2)  
  case MSEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin(r1) ++ mkeps(r2)
  case MSEQ(r1, r2) => mkfin(r2)
  case MSTAR(r) => mkfin(r) ++ List(1)
  case MNTIMES(_,_,_) => Nil
  case MZERO => Nil
  case MONE => Nil
}

def mat(r: MRexp, s: List[Char]) : MRexp = s match {
  case Nil => r
  case c::cs => mat(shift(false, r, c,List()), cs)
}

def matcher(r: Rexp, s: List[Char]) : Boolean =
  { val reg= intern2(r)
    if (s == Nil) nullable(reg)
     else fin(mat(reg, s))
  }

def matcher2(r: Rexp, s: List[Char]) : MRexp = {
  val reg= intern2(r)
  if (s == Nil)
     if(nullable(reg)) reg else MZERO 
  else mat(reg, s)
}
  
def intern(r: Rexp) : MRexp = (r: @unchecked) match{
  case ZERO            => MZERO
  case ONE             => MONE
  case CHAR(c)         => MCHAR(c, Mark()) 
  case ALT(r1, r2)     => MALT(intern(r1), intern(r2))
  case SEQ(r1, r2)     => MSEQ(intern(r1), intern(r2))
  case STAR(r)         => MSTAR(intern(r))
  case NTIMES(r, n)    => MNTIMES(intern(r), n, 0)
  //case INIT(r)         => MINIT(intern(r)) // might remove
}

def intern2(r: Rexp) : MRexp = MINIT(intern(r))

// testing one/emptystring regex 

@main
def test1() = {
  println("\n===== Testing New BitCodes =====\n")
  val rexp=SEQ(ALT(ONE,CHAR('c')) , ALT(SEQ(CHAR('c'),CHAR('c')), CHAR('c')) )
  val s="cc".toList
  val mrexp=intern2(rexp)

  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(mrexp, sPart)))
  } 

  val finReg=matcher2(rexp,s)
  val bits=mkfin(finReg)
  println(s"\n=final list= ${bits}\n")

  println(s"Final Reg:= ${finReg}\n")
  println(s"mkfin: ${mkfin(finReg)}\n")
  //println(s"Decoded value for Marked=${decode( bits, rexp)._1}\n")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  println(s"Derivatives bitcode: $derivBitcode\n")
  //println(s"Decoded value for derivatives=${decode( derivBitcode, rexp)._1}\n")
}



//testing seq,alt,char only regex
@main
def test2() = {
  println("=====Test With SEQ/ALT/CHAR only====")
  val rexp=SEQ(
    ALT(ALT(CHAR('a'),CHAR('b')),SEQ(CHAR('a'),CHAR('b'))) , 
    ALT( SEQ(CHAR('b'),CHAR('c')), ALT(CHAR('c'),CHAR('b'))) )
  val mrexp=intern2(rexp)
  val s = "abc".toList
  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(mrexp, sPart)))
  } 

  val finReg=matcher2(rexp,s)
  val bits=mkfin(finReg)
  println(s"=final list=\n ${bits} ")

  println(s"${finReg}")
  println(s"\nmkeps: ${mkeps(finReg)}")
  println(s"mkfin: ${mkfin(finReg)}")
  println(s"Decoded value for Marked=${decode( bits, rexp)._1}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  println(s"derivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${decode( derivBitcode, rexp)._1}")
}


val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}
// testing the evil regular expression
@main
def test4() = {
  for (i <- 0 to 7000000 by 500000) {
  println(f"$i: ${time_needed(2, matcher2(EVIL2, ("a" * i).toList))}%.5f")

  }
}

def pp(e: MRexp) : String = (e: @unchecked) match {
  case MZERO => "0\n"
  case MONE => "1\n"
  case MCHAR(c,mark) =>  if (mark.marked) s"${"\u001b[32m"} •$c ${mark.bs} ${"\u001b[0m"}\n" else if(mark.color==Color.RED) s"${"\u001b[31m"} $c${"\u001b[0m"}\n" else s"$c\n"
  case MALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case MSEQ(r1, r2) => "SEQ\n" ++ pps(r1, r2)
  case MSTAR(r) => "STAR\n" ++ pps(r)
}
def pps(es: MRexp*) = indent(es.map(pp))

/* 
enum Rexp {
  case ZERO 
  case ONE 
  case CHAR(c: Char, mark: Mark=Mark())
  case ALT(r1: Rexp, r2: Rexp) 
  case SEQ(r1: Rexp, r2: Rexp) 
  case STAR(r: Rexp) 
  case NTIMES(r: Rexp, n: Int , counter: Int = 0)
  case INIT(r:Rexp)
}
import Rexp._ */

/* enum VALUE {
    case EMPTY
    case CHARV(c: Char)  
    case UNMARKED
    case SEQV(v1: VALUE, r2: VALUE )
    case LEFT(v: VALUE)
    case RIGHT(v: VALUE)
    case STARV(vs: List[VALUE])
    case ERRORVALUE
    case EMPTYSTRING
}
import VALUE._ */

/* def decode(r:Rexp , bits:List[Int]): (VALUE,List[Int]) = 
  r match {
    case CHAR(c, mark) =>(CHARV(c), bits ) // to consume bits?
    case ALT(r1, r2) => bits match {
        case 0::bs => 
            val (v,bsp)= decode(r1,bs)
            (LEFT(v),bsp)
        case 1::bs =>
            val (v,bsp)= decode(r2,bs)
            (RIGHT(v),bsp)
    }
    case SEQ(r1, r2) =>
      val (v1, bs1) = decode(r1, bits)
      val (v2, bs2) = decode(r2, bs1)
      (SEQV(v1, v2), bs2) 
    case STAR(r) => bits match {
        case 8 :: bs1 => 
            (STARV(List()), bs1) // terminate recursion for STAR

        case 7 :: bs1 =>   
            val (v, bs2) = decode(r, bs1)
            val (STARV(vs), bsv) = decode(STAR(r), bs2) 
            (STARV(v :: vs), bsv) 
        case _ => 
        (STARV(List()), bits)
    
    }// end of match r   
    case ONE => (EMPTY, bits) 
    
} */

/* 
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
  case CHAR(c,mark) => if(mark.marked) s"•$c bs={${mark.bs}} color={${mark.color}}\n" else s"$c bs={${mark.bs}} color={${mark.color}}\n"
  case ALT(r1, r2) => s"ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => s"SEQ\n" ++ pps(r1, r2)
  case STAR(r) => "STAR\n" ++ pps(r)
  case NTIMES(r, n,counter) => 
    s"NTIMES{$n}{fin r=${fin(r)}, null r=${nullable(r)} counter=$counter}\n" ++ pps(r)
    s"• NTIMES {$n}{fin r=${fin(r)}, null r=${nullable(r)}, counter=$counter}\n" ++ pps(r) 
  case INIT(r) => s"INIT\n" ++ pps(r)   
}
def pps(es: Rexp*) = indent(es.map(pp))
 */