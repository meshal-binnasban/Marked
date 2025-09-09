import scala.language.implicitConversions
import $file.rexp, rexp._

type Marks = List[String]

// shifts function 
def shifts(ms: Marks, r: Rexp) : Marks = r match {
      case ZERO => Nil
      case ONE => Nil
      case CHAR(c) => for (m <- ms; if m != "" && m.head == c) yield m.tail
      case ALT(r1, r2) => (shifts(ms, r1).prune2 ::: shifts(ms, r2).prune2).prune2
      case SEQ(r1, r2) => {
        val ms1 = shifts(ms, r1).prune2
        (nullable(r1), nullable(r2)) match {
          case (true, true) =>  (shifts((ms1 ::: ms.prune2).prune2, r2) ::: ms1.prune2).prune2
          case (true, false) => shifts((ms1 ::: ms.prune2).prune2, r2).prune2 
          case (false, true) => (shifts(ms1, r2) ::: ms1).prune2
          case (false, false) => shifts(ms1, r2).prune2
        }
      }
      case STAR(r) => {
        val ms1 = shifts(ms, r).prune2
        if(ms1.isEmpty) Nil 
        else
        (ms1 ::: shifts(ms1, STAR(r)).prune2).prune2
      }

}
  

// the main matching function 
def matcher(r: Rexp, s: String) : Boolean = {
  if (s == "") nullable(r)
  else 
    shifts(List(s), r).exists(_ == "")
}

extension (ms: Marks)
  def prune2: Marks =
    var seen = Set.empty[String]
    ms.filter { s =>
      val keep = !seen.contains(s)
      if (keep) seen += s
      keep
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


