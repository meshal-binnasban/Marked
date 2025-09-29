import scala.language.implicitConversions
import $file.rexp, rexp._

type Marks = List[String]

// shifts function 
def shifts(ms: Marks, r: Rexp) : Marks = r match {
      case ZERO => Nil
      case ONE => Nil
      case CHAR(c) => for (m <- ms; if m != "" && m.head == c) yield m.tail
      case ALT(r1, r2) => (shifts(ms, r1) ::: shifts(ms, r2))
      case SEQ(r1, r2) => {
        val ms1 = shifts(ms, r1).prune2
        (nullable(r1), nullable(r2)) match {
          case (true, true) =>  (shifts((ms1 ::: ms), r2) ::: ms1)
          case (true, false) => shifts((ms1 ::: ms), r2) 
          case (false, true) => (shifts(ms1, r2) ::: ms1)
          case (false, false) => shifts(ms1, r2)
        }
      }
      case STAR(r) => {
        val ms1 = shifts(ms, r)
        if(ms1.isEmpty) Nil 
        else
        (ms1 ::: shifts(ms1, STAR(r)))
      }
      case NTIMES(r,n) if n == 0 => ms
      case NTIMES(r,n) =>
            val ms1 = shifts(ms,r)
            if(ms1.isEmpty) Nil else{
              if(nullable(r)){ ( ms1 ::: shifts(ms1,NTIMES(r,n-1)))}
              else{ (shifts(ms1,NTIMES(r,n-1))   )}
            }

}
  

// the main matching function 
def matcher(r: Rexp, s: String) : Boolean = {
  if (s == "") nullable(r)
  else
    {
      val ms=  shifts(List(s), r)
      println(ms)
      ms.exists(_ == "")
    }
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
  val r = NTIMES("a" | ONE,4)
    //(("a") | (ONE)) ~ (("a") | NTIMES("a",3))
    //aaa
    
  val s = "aaa"
  println("=string=")
  println(s)
  println(matcher(r,s))
}

@main
def test3() = {
  println("=====Test====")
  val r = %( %( "a" ) | %( "aa" ) | %( "aaa" ) | %( "aaaa" ) | %( "aaaaa" ) )
  val s = "a" * 25
  println("=string=")
  println(s)
  println(matcher(r,s))
}
 


