import scala.language.implicitConversions
import os.size

enum Color:
  case GREEN
  case RED
  case WHITE

case class Mark(
  marked: Boolean = false,
  bs: List[Int] = List(),
  color: Color = Color.WHITE
)

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
import Rexp._


def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_,_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, n,counter) => if (n == 0) true else nullable(r)
  case INIT(r) => nullable(r)
}

def fin(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => false
  case CHAR(c,marked) => marked.marked 
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
  case NTIMES(r, n,counter) => counter == n && fin(r)
  case INIT(r) => fin(r)
}

//shift char with position
def shift(m: Boolean, re: Rexp, c: Char, bits:List[Int]) : Rexp = {
  re match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d,mark) =>
    val newMark=m && d == c
    if(newMark) 
    CHAR(d, Mark(marked = newMark, bs = mark.bs ++ bits , color = mark.color))
    else  CHAR(d, Mark(marked = newMark, bs = mark.bs, color = mark.color))

  case ALT(r1, r2) => 
    ALT(shift(m, r1, c,0::bits)
        , shift(m, r2, c,1::bits) )
  case SEQ(r1, r2) => 
    SEQ(shift(m, r1, c,bits)
        , shift(m && nullable(r1) || fin(r1) ,r2, c, bits))
  case STAR(r) => 
    STAR(shift(m || fin(r), r, c,7::bits))
  case NTIMES(r, n,counter) => 
    if (counter == n) re else{
        if (m || fin(r)) NTIMES(shift(m || fin(r), r, c,bits), n, counter+1)
        else NTIMES(shift(false, r, c, bits), n, counter)       
        }  
  case INIT(r) => 
    println("in Here")
    shift(true, r, c,bits)  
    } 
    }

def mat(r: Rexp, s: List[Char]) : Rexp = s match {
  case Nil => r
  case c::cs => mat(shift(false, r, c,List()), cs)
}

def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s))

def matcher2(r: Rexp, s: List[Char]) : Rexp =
  if (s == Nil)
     if(nullable(r)) r else ZERO 
  else mat(r, s)

def intern2(r: Rexp) : Rexp = INIT(r)

enum VALUE {
    case EMPTY  
    case UNMARKED(c: Char)
    case SEQV(v1: VALUE, r2: VALUE )
    case LEFT(v: VALUE)
    case RIGHT(v: VALUE)
    case STARV(vs: List[VALUE])
    case ERRORVALUE
}
import VALUE._

def mkeps(r: Rexp): List[Int] = r match {
  case ONE => Nil
  case CHAR(_, mark) =>
    if (mark.marked) mark.bs else Nil
  case ALT(r1, r2) =>
    if (fin(r1)) mkeps(r1)  
    else mkeps(r2)
  case SEQ(r1, r2) =>
    mkeps(r1) ++ mkeps(r2)
  case STAR(r) =>
    mkeps(r)++List(8)
  case NTIMES(r1, _, _) =>
    mkeps(r1)
  case INIT(r1) =>
    mkeps(r1)
  case _ => Nil
}

def decode(r:Rexp , bits:List[Int]): (VALUE,List[Int]) = r match {
/*     case CHAR(c,mark) => 
        if(mark.marked) (EMPTY,bits) else (UNMARKED(c),bits) // case of 1 as in matched and case of CHAR combined from original decode
 */
    case CHAR(c, _) =>
        if bits.isEmpty then
            (EMPTY, bits)
        else 
            (UNMARKED(c), List())

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

    case _ => (ERRORVALUE, bits) 
    
}


@main
def test1() = {
  println("\n===== Testing New PopPoints =====\n")
  //1 + ((1 + 1) + 1)
  val rexp = ("a" | ( ("b"|"c")  | "d") )
  val initRexp=intern2(rexp)
  val s="c".toList
  println(s"String: $s\n")
  val finReg=matcher2(initRexp, s)
  println(s"Original size=${size(initRexp)} Result= ${fin(finReg)} Final Size=${size(finReg)}")
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(initRexp, sPart)))
  }

  println(s"\nfinReg=${finReg}\n")

  println("\n ====== Testing Decode  ======\n")


  //val bits=List(1,0,1)
  val bits=mkeps(finReg)
  println(s"mkeps value= $bits")
  
  val (decodeValue,remainingBits)=decode(rexp,bits)
  println(s"Dcode: \nvalue=$decodeValue \nremaining bits=$remainingBits")

  /* println("\n=== Testing popPoints with shift ===\n")
  val r=STAR("a" | "b" | "c" )
  val shiftedR=shift(Mark(true,List(),Color.RED),r,('a'))
  val shiftedRTwice=shift(Mark(true,List(),Color.RED),shiftedR,('b'))
  val shiftedThrice=shift(Mark(true,List(),Color.RED),shiftedRTwice,('c'))
  println(r)
  println(s"original r=\n${pp(r)}")
  println(s"1- Shifted with 'a', r=\n${pp(shiftedR)}")
  println(s"2- Shifted with 'b', r=\n${pp(shiftedRTwice)}")
  println(s"3- Shifted with 'c', r=\n${pp(shiftedThrice)}")
   */
}


@main
def test2() = {
  // test and fix star case
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
def test3() = {
  for (i <- 0 to 7000000 by 500000) {
  
  println(f"$i: ${time_needed(2, matcher2(EVIL2, ("a" * i).toList))}%.5f")

  }
}


def size(r: Rexp) : Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_,_) => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r) => 1 + size(r)
  case NTIMES(r,n,counter) => 1 + size(r)
  case INIT(r) => 1 + size(r) 
}

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
