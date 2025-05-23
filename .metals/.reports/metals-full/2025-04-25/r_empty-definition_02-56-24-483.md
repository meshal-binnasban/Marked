error id: _empty_/PriorityType.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/rexp.sc
empty definition using pc, found symbol in pc: 
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -PriorityType.PriorityType.
	 -Color.PriorityType.
	 -Rexp.PriorityType.
	 -VALUE.PriorityType.
	 -PriorityType.
	 -scala/Predef.PriorityType.
offset: 60
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/rexp.sc
text:
```scala
enum PriorityType:
  case A
  case B
  case N
import Pri@@orityType._
case class Priority(
  p: PriorityType = N,
  pOrder: Int = 0,
  color:
)
enum Color:
  case GREEN
  case RED
  case WHITE
import Color._



// standard regexes
enum Rexp {
  case ZERO 
  case ONE 
  case CHAR(c: Char) 
  case ALT(r1: Rexp, r2: Rexp) 
  case SEQ(r1: Rexp, r2: Rexp) 
  case STAR(r: Rexp) 
  case NTIMES(r: Rexp, n: Int)
  case NOT(r: Rexp) 
  case POINT(bs: Bits, r: Rexp)
  case POINTS(bs: Bits, r: Rexp, p:Priority)
  case INIT(r: Rexp)
}
import Rexp._

enum VALUE {
    case EMPTY
    case CHARV(c: Char)  
    case SEQV(v1: VALUE, r2: VALUE )
    case LEFT(v: VALUE)
    case RIGHT(v: VALUE)
    case STARV(vs: List[VALUE])
    case ERRORVALUE(msg:String)
}
import VALUE._

def size(r: Rexp) : Int = (r: @unchecked) match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_) => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r) => 1 + size(r)
  case NTIMES(r,n) => 1 + size(r)
  case INIT(r) => 1 + size(r)
}

// some syntax sugar for regexes
import scala.language.implicitConversions

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
}
def pps(es: Rexp*) = indent(es.map(pp))

// Decode function to reconstruct match structure
def decode(bs: List[Int], r: Rexp): (VALUE, List[Int]) = (r: @unchecked) match {
  case ONE => (EMPTY, bs)
  case CHAR(c) => (CHARV(c), bs)
  case ALT(r1, r2) => bs match {
    case 0 :: rest => val (v, rem) = decode(rest, r1); (LEFT(v), rem)
    case 1 :: rest => val (v, rem) = decode(rest, r2); (RIGHT(v), rem)
    case _ => (ERRORVALUE("ALT ERROR"), bs)
  }
  case SEQ(r1, r2) =>
    val (v1, bs1) = decode(bs, r1)
    val (v2, bs2) = decode(bs1, r2)
    (SEQV(v1, v2), bs2)

  case STAR(r) => bs match {
    case 1 :: rest => (STARV(List()), rest)
    case 0 :: rest => 
      val (v, bs1) = decode(rest, r)
      val (STARV(vs), bs2) = decode(bs1, STAR(r)): @unchecked
      (STARV(v :: vs), bs2)
    case _ => (ERRORVALUE("ALT ERROR"), bs)
  }
}

abstract class Bit
case object Z extends Bit {
  override def toString = "0"
}
case object S extends Bit {
  override def toString = "1"
}
case object C extends Bit {
  override def toString = "7"
}
case object E extends Bit {
  override def toString = "8"
}
case object SE1 extends Bit {
  override def toString = "2"
}
case object SE2 extends Bit {
  override def toString = "3"
}
case object ST1 extends Bit {
  override def toString = "4"
}
case object ST2 extends Bit {
  override def toString = "5"
}

type Bits = List[Bit]

val bitWeight: Bit => Double = {
  case C   => 100.0
  case Z   => 0
  case S   => -0.25
  case SE1 => -0.25
  case SE2 => -0.25
  case ST1 => -0.25
  case ST2 => -0.25
  case E   => -2.0

}

def totalBitsWeight(bs: Bits): Double = {
  val n = bs.length
  if (n == 0) return 0.0
  val weightedSum = bs.zipWithIndex.map {
    case (bit, idx) => bitWeight(bit) / (idx + 1) 
  }.sum

  (weightedSum) * 100.0 // average reward per C, scaled to percentage
}

def bitsToInts(bs: Bits): List[Int] = bs.map {
  case Z    => 0
  case S    => 1
  case SE1  => 2
  case SE2  => 3
  case C    => 7
  case E    => 8
  case ST1  => 4
  case ST2  => 5
}

def intsToBits(bs: List[Int]): Bits = bs.map {
  case 0 => Z
  case 1 => S
  case 2 => SE1
  case 3 => SE2
  case 4 => ST1
  case 5 => ST2
  case 7 => C
  case 8 => E
}


def convertMtoDBit2(bs: Bits): Bits = bs.flatMap {
    case ST1 => Some(Z)  // ST1 (4) => 0
    case ST2 => Some(S)  // ST2 (5) => 1
    case Z   => Some(Z)  //  0 => 0
    case S   => Some(S)  //  1 => 1
    case _   => None     // discard 
  }




/* implicit val bitOrdering: Ordering[Bit] = Ordering.by {
  case C   => 0  // high priority for matching char?
  case E   => 1
  case Z   => 2
  case S   => 3
  case SE1 => 3
  case SE2 => 3
} */

```


#### Short summary: 

empty definition using pc, found symbol in pc: 