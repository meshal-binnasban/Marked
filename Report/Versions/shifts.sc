
import scala.language.implicitConversions

// regular expressions
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp
case class NTIMES(r:Rexp , n:Int) extends Rexp

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
  case NTIMES(r,n) => if (n==0) true else nullable(r)
}

type Marks = List[String]

// shifts function 
def shifts(ms: Marks, r: Rexp) : Marks = r match {
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
    if(ms1.isEmpty) ms1 
    else
    ms1 ::: shifts(ms1, STAR(r)) 
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
  val r = %("a") 
  val s = "aaa"
  println(r)
  println(s)
  println(s"res: ${matcher(r, s)}")
}

@main
def test2() = {
  println("=====Test====")
  val r = (ONE | "c") ~ %(%("c"))
  val s = "cccc"
  println(r)
  println(s)
  println(s"res: ${matcher(r, s)}")
}


@main
def test3() = {
  println("=====Test====")
  val r = SEQ(CHAR('a'),ALT(ONE,ONE))
  val s = "a"
  println("=string=")
  println(r)
  println(s)
  println(s"res: ${matcher(r, s)}")
}

@main
def test4() = {
  println("=====Test====")
  val r = (%(%(ONE))) ~ "c"
  val s = "c"
  println(r)
  println(s)
  println(s"res: ${matcher(r, s)}")
}

@main
def test5() = {
  println("=====Test====")
  val r = STAR("e" | ("d" | "c"))
  val s = "cdc"
  println("=string=")
  println(r)
  println(s)
  println(s"res: ${matcher(r, s)}")
}

@main
def test6() = {
  println("=====Test====")
  val r = STAR(ONE ~ ONE) ~ "cc"
  val s = "cc"
  println("=string=")
  println(r)
  println(s)
  println(s"res: ${matcher(r, s)}")
}

@main
def test7() = {
  println("=====Test====")
  val r = "b" | "ba"
  val s = "ba"
  println(r)
  println(s)
  println(s"res: ${matcher(r, s)}")
}


