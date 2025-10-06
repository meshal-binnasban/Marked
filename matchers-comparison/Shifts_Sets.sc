import scala.language.implicitConversions
import $file.rexp, rexp._

type Marks = Set[String]
type MarksPosition = Set[Int]

var S: String = "" 

// shifts function
def shifts(ms: MarksPosition, r: Rexp): MarksPosition = r match {
  case ZERO => Set.empty[Int]
  case ONE  => Set.empty[Int]
  case CHAR(c) =>// for (m <- ms; if m != "" && m.head == c) yield m.tail
                    val len = S.length
                    for (p <- ms  if p < len  && S.charAt(p) == c) yield p + 1
  case ALT(r1, r2) => shifts(ms, r1) ++ shifts(ms, r2)
  case SEQ(r1, r2) => {
    val ms1 = shifts(ms, r1)
    (nullable(r1), nullable(r2)) match {
      case (true,  true)  => shifts(ms1 ++ ms, r2) ++ ms1
      case (true,  false) => shifts(ms1 ++ ms, r2)
      case (false, true)  => shifts(ms1, r2) ++ ms1
      case (false, false) => shifts(ms1, r2)
    }
  }
  case STAR(r) => {
    val ms1 = shifts(ms, r)
    if (ms1 == Set()) ms1 else ms1 ++ shifts(ms1, STAR(r))
  }
  case NTIMES(r, n) =>
    if (n == 0) ms
    else {
      val ms1 = shifts(ms, r)
      println(s"ms1= $ms1")
      if (ms1 == Set()) ms1
      else if (nullable(r)) ms1 ++ shifts(ms1, NTIMES(r, n - 1))
      else shifts(ms1, NTIMES(r, n - 1))
    }
}

// the main matching function 
def matcher(r: Rexp, s: String): Boolean =
  S=s
  if (s == "") nullable(r)
  else shifts(Set(0), r).contains(S.length)
  /* if (s == "") nullable(r)
  else shifts(Set(s), r).contains("") */

def mat(r: Rexp, s: String): MarksPosition =  
  S = s
  shifts(Set(0), r)
  //shifts(Set(s), r)


@main
def test1() = {
  println("=====Test====")
  val r = NTIMES("a",2)
    //(("a") | (ONE)) ~ (("a") | NTIMES("a",3))
    //aaa
    
  val s = "aa"
  println("=string=")
  println(s)
  //println(matcher(r,s))
  println(mat(r,s))
}

@main
def test2() = {
  println("=====Test====")
  val r = %( %( "a" ) | %( "aa" ) | %( "aaa" ) | %( "aaaa" ) | %( "aaaaa" ) )
  val s = "a" * 700
  println("=string=")
  println(s)
  println(matcher(r,s))
}
 


