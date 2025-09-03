
import scala.language.implicitConversions

// regular expressions
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp

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
}

type Marks = List[String]

// shifts function 
def shifts(ms: Marks, r: Rexp) : Marks = simp(r) match {
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
        val ms1 = shifts(ms, r).prune2
        if(ms1.isEmpty) ms1
        else
       ( ms1 ::: shifts(ms1, STAR(r))).prune2 
      }
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

def simp(r: Rexp): Rexp = r match {
  case ALT(r1, r2) =>
    (simp(r1), simp(r2)) match {
      case (ZERO, r2s)      => r2s
      case (r1s, ZERO)      => r1s
      case (x, y) if x == y => x
      case (r1s, r2s)       => ALT(r1s, r2s)
    }
  case SEQ(r1, r2) =>
    (simp(r1), simp(r2)) match {
      case (ZERO, _)  => ZERO
      case (_, ZERO)  => ZERO
      case (ONE, r2s) => r2s
      case (r1s, ONE) => r1s
      case (r1s, r2s) => SEQ(r1s, r2s)
    }
  case STAR(r1) =>
    simp(r1) match {
      case ZERO        => ONE
      case ONE         => ONE
      case STAR(inner) => STAR(inner)     
      case r1s         => STAR(r1s)      
    }
  case r => r
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
  val r = %("aa") ~ %(%("a"))
  val s = "aaaaaa"
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
  val r = (%(%(ONE ~ ONE))) ~ "c"
  val s = "c"
  println(r)
  println(s)
  println(s"res: ${matcher(r, s)}")
}

@main
def test8() = {
  println("=====Test====")
  val r=  %( %("a") ) //~ "b"
  val s = "a" * 2263 //+ "b"
  println(s"Regex:\n${(r)}\n")
  println(s"=string=\n$s")
  println(s"res: ${matcher(r, s)}")
}
