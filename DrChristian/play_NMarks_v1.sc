
import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit

type Marks = List[List[Char]]
def shift(ms: Marks, r: Rexp) : Marks = {
  if (ms == Nil) Nil
  else (r: @unchecked) match {
    case ZERO => Nil
    case ONE => Nil
    case CHAR(d) => for (m <- ms if m != Nil && m.head == d) yield m.tail
    case ALT(r1, r2) => shift(ms, r1) ::: shift(ms, r2)
    case SEQ(r1, r2) => 
      if (nullable(r1)) {
        if (nullable(r2)) shift(shift(ms, r1) ::: ms, r2) ::: shift(ms, r1)
        else shift(shift(ms, r1) ::: ms, r2)
      }  
      else {
        if (nullable(r2)) shift(shift(ms, r1), r2) ::: shift(ms, r1)
        else shift(shift(ms, r1), r2)
      }     
    case STAR(r) => shift(shift(ms, r), STAR(r)) ::: ms
  }
}

def matcher(r: Rexp, s: String) : Boolean =
  shift(List(s.toList),r).exists(_ == Nil)

//(ONE | "a" ) ~ ( "a" | "aa" )
@main 
def test1() = {
  println("=====Test====")
  //val br2 = (ONE | "a" ) ~ ( "a" | "aa" )
  val br2= (ONE | "a") ~ ("a" | "aa")
  val s = "a"

  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")
  val markedMatcher=matcher(br2, s)
  val derMatcher= rebit.matcher(br2, s)

  if(markedMatcher != derMatcher) {
    println(s"Mismatch: Marked=${markedMatcher}, Derivative=${derMatcher}")
  } else {
    println(s"Matchers Equal=$markedMatcher")
  }   
}

//("a" | "ab") ~ ("c" | "bc")
@main
def test2() = {
  println("=====Test====")
  val br2= ("a" | "ab") ~ ("c" | "bc")
    //%("aa") | ("aa" ~ ONE)
  val s = "abc"

  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")
  val markedMatcher=matcher(br2, s)
  val derMatcher= rebit.matcher(br2, s)

  if(markedMatcher != derMatcher) {
    println(s"Mismatch: Marked=${markedMatcher}, Derivative=${derMatcher}")
  } else {
    println(s"Matchers Equal=$markedMatcher")
  }
}

//(("a"|"b") | ("ab")) ~ (("bc") | ("c" | "b"))
@main
def test3() = {
  println("=====Test====")
  val br2= (("a"|"b") | ("ab")) ~ (("bc") | ("c" | "b"))
    //%("aa") | ("aa" ~ ONE)
  val s = "abc"

  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")
  val markedMatcher=matcher(br2, s)
  val derMatcher= rebit.matcher(br2, s)

  if(markedMatcher != derMatcher) {
    println(s"Mismatch: Marked=${markedMatcher}, Derivative=${derMatcher}")
  } else {
    println(s"Matchers Equal=$markedMatcher")
  }
}

//( ONE | ( ONE | "bc" )  )  | ( "a"| ONE ) ~ ("a" | "aa")
@main
def test4() = {
  println("=====Test====")
  val br2= ( ONE | ( ONE | "bc" )  )  | ( "a"| ONE ) ~ ("a" | "aa")
  val s = "aa"

  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")
  val markedMatcher=matcher(br2, s)
  val derMatcher= rebit.matcher(br2, s)
  
  if(markedMatcher != derMatcher) {
    println(s"Mismatch: Marked=${markedMatcher}, Derivative=${derMatcher}")
  } else {
    println(s"Matchers Equal=$markedMatcher")
  }
}

@main
def test5() = {
  println("=====Test====")
  val br2= ( ("a" | "b") ~ (ONE | "a") ) ~ "a"
  val s = "aa"

  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")
  val markedMatcher=matcher(br2, s)
  val derMatcher= rebit.matcher(br2, s)

  if(markedMatcher != derMatcher) {
    println(s"Mismatch: Marked=${markedMatcher}, Derivative=${derMatcher}")
  } else {
    println(s"Matchers Equal=$markedMatcher")
  }
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
  case ONE => s"1 \n"
  case CHAR(c) => s"$c\n"
  case ALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => "SEQ\n" ++ pps(r1, r2)
  case STAR(r) => s"STAR\n" ++ pps(r)
  case NTIMES(r, n) => s"NTIMES($n)\n" ++ pps(r)
}

def pps(es: Rexp*) = indent(es.map(pp))

// extracts a string from a value
def flatten(v: Val) : String = v match {
   case Empty => ""
   case Chr(c) => c.toString
   case Left(v) => flatten(v)
   case Right(v) => flatten(v)
   case Sequ(v1, v2) => flatten(v1) ++ flatten(v2)
   case Stars(vs) => vs.map(flatten).mkString
   case Nt(vs, _) => vs.map(flatten).mkString
   //case Rec(_, v) => flatten(v)
 }


import scala.collection.parallel.CollectionConverters._

@main
def strongTestNoSTARParallel() = {
  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE),
        (0, _ => ZERO),
        (0, _ => CHAR('a')),
        (0, _ => CHAR('b')),
        (0, _ => CHAR('c')),
       // (1, cs => STAR(cs(0))),
        (2, cs => ALT(cs(0), cs(1))),
        (2, cs => SEQ(cs(0), cs(1)))
      )
  val alphabet = LazyList('a', 'b')
  
  val numRegexes = BigInt(10_000_000_000L)
  val batchSize = BigInt(100_000L) 
  
  val batches = (BigInt(0) to numRegexes by batchSize).toVector.par
  batches.foreach { start =>
    val end = (start + batchSize - 1).min(numRegexes)
    for (i <- start to end) {
      val r = enumerate.decode(i)
      if (i % 100_000 == 0) { print("*") }
      for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "") {
          val markedMatcher=matcher(r, s)
          val derMatcher= rebit.matcher(r, s)
          if (markedMatcher != derMatcher) {
            println(s"[${i}]-\n reg: $r \nstr: $s")
            println(s"\n${pp(r)}")
            print("Type 'N' to exit, anything else to continue: ")
            val input = scala.io.StdIn.readLine()
            if (input.trim.toLowerCase == "n") {
                System.exit(1)
            }//end of if n -> exit
        }//end of if markedMatcher != derMatcher
      }// end of for s <- regenerate.generate_up_to(alphabet)(10)(r)
    }//end of for i <- start to end
  }// end of batches.foreach
  println("\nAll tests passed!")
}// end of strongTestNoSTARParallel

