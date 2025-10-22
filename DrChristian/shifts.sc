
import scala.language.implicitConversions
import $file.Derivatives
//import $file.rexp, rexp._

// regular expressions
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp
case class AND(r1: Rexp, r2: Rexp) extends Rexp

def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

// strings are coerced into Rexps
given Conversion[String, Rexp] = (s => charlist2rexp(s.toList))

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
  case NTIMES(r,n) => if n == 0 then true else nullable(r)
  case AND(r1, r2) => nullable(r1) && nullable(r2)
}

type Marks = List[String]

// shifts function 
def shifts(ms: Marks, r: Rexp) : Marks = 
  r match {
      case ZERO => Nil
      case ONE => Nil
      case CHAR(c) => for (m <- ms; if m != "" && m.head == c) yield m.tail
      case ALT(r1, r2) => shifts(ms, r1) ::: shifts(ms, r2)
      case SEQ(r1, r2) => {
        val ms1 = shifts(ms, r1)
        (nullable(r1), nullable(r2)) match {
          case (true, true) =>  shifts(ms1 ::: ms, r2) ::: ms1
          case (true, false) => shifts(ms1 ::: ms, r2) 
          case (false, true) => shifts(ms1, r2) ::: ms1
          case (false, false) => shifts(ms1, r2)
        }
      }
      case STAR(r) => {
        val ms1 = shifts(ms, r)
        println(s"ms in star=$ms1")
        if(ms1.isEmpty) Nil
        else{
        val msr= ( ms1 ::: shifts(ms1, STAR(r)) )
        println(s"ms returned= $msr")
        msr
        }
      }
      case NTIMES(r,n) =>
        if(n==0) Nil
        else if(n==1) shifts(ms,r)
        else{
          val ms1 = shifts(ms,r)
          if(ms1.isEmpty) ms1
          else
              if(nullable(r)) ms1 ::: shifts(ms1,NTIMES(r,n-1))
              else shifts(ms1,NTIMES(r,n-1))
        }
      case AND(r1,r2) => (shifts(ms,r1).intersect(shifts(ms,r2)))
}

extension (ms: Marks)
  def reshuffle: Marks = ms.sortBy(m => (m.length))
  def prune2: Marks = 
    var seen = Set.empty[String]
    ms.filter { m =>
      val keep = !seen.contains(m)
      if (keep)
        seen += m
      keep
  }

// the main matching function 
def matcher(r: Rexp, s: String) : Boolean = {
  if (s == "") nullable(r)
  else 
    println(s"List Marks = ${shifts(List(s), r)}") 
    shifts(List(s), r).exists(_ == "")
}

@main
def test1() = {
  println("=====Test====")
  val r=  %( %("a") ) //~ "b"
  val s = "a" * 920 //+ "b"
  println(s"Regex:\n${(r)}\n")
  println(s"=string=\n$s")
  println(s"res: ${matcher(r, s)}")
}

@main
def test2() = {
  println("=====Test====")
  //val r=   AND(%("a") , %("aa"))
  val r1= %("aa" | "b")
  val r2= %("a" | "b") ~ "b"

  val r= AND(r1 , r2) 

  val s = "aabaab" 
  println(s"Regex:\n${(r)}\n")
  println(s"=string=\n$s")
  val sResult=matcher(r, s)
 // val dResult=Derivatives.matcher(r, s)
  println(s"Shifts: ${sResult}")
 // println(s"Derivatives: ${dResult}")
  //if(sResult != dResult) println("Mismatch!") else println("Match!")


}

@main
def test3() = {
  println("=====Test====")
  val r= (%("a") ~ "a") ~ "a"
  val s = "aaa" 
  println(s"Regex:\n${(r)}\n")
  println(s"=string=\n$s")
  val sResult=matcher(r, s)
  println(s"Shifts: ${sResult}")
  
}


@main
def testall() = {
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

  for (i <- 0L to 100_000_000L) {
    val r = decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- (generate_up_to(alphabet)(20)(r).take(19)) if s != "") {
      val res = matcher(r, s)
      if (!res) {
        println(s"$r and $s")
        System.exit(1)
      }
    }
  }
}