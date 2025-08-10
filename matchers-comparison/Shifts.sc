import scala.language.implicitConversions
import $file.rexp, rexp._

type Marks = List[String]

// shifts function 
def shifts(ms: Marks, r: Rexp) : Marks = 
  if (ms == Nil) Nil else {
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
        ms1 ::: shifts(ms1, STAR(r)) 
      }
    }
  }

// the main matching function 
def matcher(r: Rexp, s: String) : Boolean = {
  if (s == "") nullable(r)
  else 
    shifts(List(s), r).exists(_ == "")
}

@main
def test1() = {
  println("=====Test====")
  val r = ("a" | "ab") ~ ("c" | "bc")
  val s = "abc"
  println("=string=")
  println(s)
  println(matcher(r,s))
}


