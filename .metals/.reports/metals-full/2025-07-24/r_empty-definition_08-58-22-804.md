error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/play_NMarks_Exp.sc:
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/play_NMarks_Exp.sc
empty definition using pc, found symbol in pc: 
empty definition using semanticdb
empty definition using fallback
non-local guesses:

offset: 20384
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/play_NMarks_Exp.sc
text:
```scala

import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit


extension (xs: List[Bits]) {
  def <*> (ys: List[Bits]): List[Bits] =
    for (x <- xs; y <- ys) yield x ::: y

  def <+> (y: Bit): List[Bits] =
    for (x <- xs) yield x :+ y

  def <++>(ys: Bits): List[Bits] =
    xs.map(_ ++ ys) 

  def <::>(y: Bit): List[Bits] = 
    for (x <- xs) yield y :: x
}

extension (ms: List[Mark]) {
  def <:+>(b: Bit): List[Mark] = 
    ms.map(m => m.copy(bits = m.bits :+ b))
  def <::+>(bs: Bits): List[Mark] = 
    ms.map(m=> m.copy(bits= m.bits ::: bs))
}

def mkeps2(r: Rexp) : Bits = r match {
  case ONE => Nil//List(Ep)
  case ALT(r1, r2) =>if (nullable(r1)) Lf :: mkeps2(r1) else Ri :: mkeps2(r2)
  case SEQ(r1, r2) => mkeps2(r1) ++ mkeps2(r2)
  case STAR(r,m) => List(En)
}

type Marks = List[Mark]
def shift(ms: Marks, r: Rexp, consumed: Boolean = false): (Marks, Boolean) = 
    //println(s"${pp(r)}\nms=$ms")
    if (ms == Nil) (Nil, false)
    else (r: @unchecked) match {
        case ZERO => (Nil, false)
        case ONE => (Nil, false)
        case CHAR(d) =>
            val cms = for (m <- ms if m.str != Nil && m.str.head == d) yield m.copy(str = m.str.tail)
            (cms, cms.nonEmpty)
        case ALT(r1, r2) =>
            val (lms, c1) = shift(ms <:+> Lf, r1, consumed)
            val (rms, c2) = shift(ms <:+> Ri, r2, consumed)
            ((lms ::: rms).reshuffle.prune, c1 || c2)

        case SEQ(r1, r2) if nullable(r1) && nullable(r2) =>
            val (r1ms, c1) = shift(ms, r1, consumed)
            val r2Lists = (r1ms ::: (ms <::+> mkeps2(r1))).reshuffle.map(m => shift(List(m), r2, false))
            val r2ms = r2Lists.flatMap(_._1)
            val c2 = r2Lists.exists(_._2)
            (((r1ms <::+> mkeps2(r2)).reshuffle ::: r2ms).reshuffle.prune, c1 || c2)

        case STAR(r, m) =>
            val (rMarks,c1)=shift( (ms <:+> Nx ), r, consumed)
            
            if (c1)//maybe change one and have it same as before
                val starList = rMarks.reshuffle.map(m => shift(List(m), STAR(r), c1))
                val sms = starList.flatMap(_._1) ::: (ms<:+> En)
                val cs = starList.exists(_._2)
                //println(s"inConsume, sms= $sms\nconsume r=$cs")
                (sms.reshuffle, cs)
                else {
                    //println(s"in else consumes,ms=$ms")
                    ( ( (ms<:+> En) ::: (rMarks<:+>En) ).reshuffle , false)
                    }
        }// ((shift((rMarks <:+> En) ,r,consumed))._1 ::: (ms<:+> En).reshuffle , false)



extension (ms: List[Mark])
  def reshuffle: List[Mark] = ms.sortBy(m => (m.str.length))
  def prune: List[Mark] =
    var seenEmpty = false
    ms.collect {
        case m if m.str != Nil => m
        case m if m.str == Nil && !seenEmpty =>
            seenEmpty = true
            m
            }

def matcher(r: Rexp, s: String) : Boolean =
  val im = Mark(bits = List(), str = s.toList)
  val (marks, consumedFlag) = shift(List(im), r)
  println(s"Last Marks($consumedFlag)\n${marks.foreach(m => println(s"-$m"))}")
  marks.exists(_.str == Nil)

def lex(r: Rexp, s: String): Option[Bits] =
  if matcher(r, s) then
    if s.toList == Nil then Some(mkeps2(r))
    else Some(shift(List(Mark(bits = List(), str = s.toList)), r)._1.head.bits)
  else None

def lexMarks(r: Rexp, s: String): Option[Marks] =
  if matcher(r, s) then
    if s.toList == Nil then 
        println("Empty string, not returning marks for now, later will adjust mkeps to return the empty maybe?")
        None
    else Some(shift(List(Mark(bits = List(), str = s.toList)), r)._1)
  else None

/* def lexer(r: Rexp, s: List[Char]) : Option[List[Val]] = {
  lex(r, s).map(_.map(dec2(r, _)))
} */


// %( "a" | "aa" )
@main 
def test1() = {
  println("=====Test====")
  val br2= %( "a" | "aa" )
  val s = "aa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")
  println("=========================")
  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}") 
  println("=========================")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

// %(%("a"))
@main 
def test2() = {
  println("=====Test====")
  //val br2= %(%("a"))
  //val s = "a" * 900
  val br2= %("a")
  val s= "aaa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")
  println("=========================")
  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}")
  println("=========================")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")  
}

//%("aa") ~ %(%("a")) 
@main 
def test3() = {
  println("=====Test====")
  val br2= %("aa") ~ %(%("a"))
  val s = "aaaaaa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")
  println("=========================")
  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}") 
  println("=========================")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")  
}

//( %("a") ~ %("a") ) ~ "a"
@main 
def test4() = {
  println("=====Test====")
  val br2= ( %("a") ~ %("a") ) ~ "a"
  val s = "aaa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)
  println("=========================")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")
  println("=========================")
  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}") 
  println("=========================")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

//%( %( %("a") ) ) 
@main 
def test5() = {
  println("=====Test====")
  val br2= %( %( %("a") ) ) 
  val s = "aa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)
  println("=========================")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")
  println("=========================")
  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}") 
  println("=========================")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

// %("a" ~ %("a") )
@main 
def test6() = {
  println("=====Test====")
  val br2= %("a" ~ %("a") )
  val s = "aaa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)
  println("=========================")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")
  println("=========================")
  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}") 
  println("=========================")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

// %( %("a") ~ "a" )
@main 
def test7() = {
  println("=====Test====")
  val br2= %( %("a") ~ "a" )
  val s = "aaa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)
  println("=========================")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")
  println("=========================")
  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}") 
  println("=========================")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

// %("a") ~ (%("a") ~ "a" )
@main 
def test8() = {
  println("=====Test====")
  val br2= %("a") ~ (%("a") ~ "a" )
  val s = "aaa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)
  println("=========================")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")
  println("=========================")
  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}") 
  println("=========================")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

// %("a") ~ %("a")
@main 
def test9() = {
  println("=====Test====")
  val br2= %("a") ~ %("a")
  val s = "a"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)
  println("=========================")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")
  println("=========================")
  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}") 
  println("=========================")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}
// %(ONE | "a")
@main 
def test10() = {
  println("=====Test====")
  val br2= %(ONE | "a")
  val s = "a" * 10
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)
  println("=========================")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")
  println("=========================")
  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}") 
  println("=========================")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}


//(ONE | "a") ~ ("a" | "aa")
@main 
def test11() = {
  println("=====Test====")
  val br2= (ONE | "a") ~ ("a" | "aa")
  val s = "aaa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)
  println("=========================")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")
  println("=========================")
  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}") 
  println("=========================")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

//("a" | "ab") ~ ("c" | "bc")
@main
def test12() = {
  println("=====Test====")
  val br2= ("a" | "ab") ~ ("c" | "bc")
  val s = "abc"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)
  println("=========================")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")
  println("=========================")
  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}") 
  println("=========================")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

//(("a"|"b") | ("ab")) ~ (("bc") | ("c" | "b"))
@main
def test13() = {
  println("=====Test====")
  val br2= (("a"|"b") | ("ab")) ~ (("bc") | ("c" | "b"))
  val s = "abc"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)
  println("=========================")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")
  println("=========================")
  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}") 
  println("=========================")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

//( ONE | ( ONE | "bc" )  )  | ( "a"| ONE ) ~ ("a" | "aa")
@main
def test14() = {
  println("=====Test====")
  val br2= ( ONE | ( ONE | "bc" )  )  | ( "a"| ONE ) ~ ("a" | "aa")
  val s = "aa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)
  println("=========================")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")
  println("=========================")
  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}") 
  println("=========================")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}
@main
def test15() = {
  println("=====Test====")
  val br2= ( ("a" | "b") ~ (ONE | "a") ) ~ "a"
  val s = "aa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)
  println("=========================")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")
  println("=========================")
  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}") 
  println("=========================")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

// decoding of a value from a bitsequence
def decode_aux(r: Rexp, bs: Bits) : (Val, Bits) = ((r, bs): @unchecked) match {
  case (ONE, bs) => (Empty, bs)
  case (CHAR(c), bs) => (Chr(c), bs)
  case (ALT(r1, r2), Lf::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    (Left(v), bs1)
  }
  case (ALT(r1, r2), Ri::bs) => {
    val (v, bs1) = decode_aux(r2, bs)
    (Right(v), bs1)
  }
  case (SEQ(r1, r2), bs) => {
    val (v1, bs1) = decode_aux(r1, bs)
    val (v2, bs2) = decode_aux(r2, bs1)
    (Sequ(v1, v2), bs2)
  }
  case (STAR(_,_), En::bs) => (Stars(Nil), bs)

  case (STAR(r1,m), Nx::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    val (Stars(vs), bs2) = (decode_aux(STAR(r1), bs1)  : @unchecked)
    (Stars(v::vs), bs2)
  }
  
  case (NTIMES(r1,n), NxT::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    val (Nt(vs,ns), bs2) = (decode_aux(NTIMES(r1,n-1), bs1)  : @unchecked)
    (Nt(v::vs,n), bs2)
  }
  case (NTIMES(_,_), EnT::bs) => (Nt(Nil,0), bs)
}

def dec2(r: Rexp, bs: Bits) = decode_aux(r, bs) match {
  case (v, Nil) => v
  case _ => throw new Exception("Not decodable")
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
  case STAR(r,m) => s"STAR, m=$m\n" ++ pps(r)
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
def strongTestParallel() = {
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
  
  val numRegexes = BigInt(10_000_000_000L)
  val batchSize = BigInt(100_000L) 
  
  val batches = (BigInt(0) to numRegexes by batchSize).toVector.par
  batches.foreach { start =>
    val end = (start + batchSize - 1).min(numRegexes)
    for (i <- start to end) {
      val r = enumerate.decode(i)
      if (i % 100_000 == 0) { print("*") }
      for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "") {
          val markedBits=lex(r, s).getOrElse(Nil)
          val derBits= rebit.lex(r, s.toList)
          if (markedBits != derBits) {
            println(s"[${i}]-\n reg: $r \nstr: $s")
            println(s"\n${pp(r)}")
            println(s"markedBits= $markedBits\nderBits=$derBits")
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

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

@main
def testingLoop() = {
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
  
  val numRegexes = BigInt(10_000_000_000L)
  val batchSize = BigInt(100_000L) 
  
  val batches = (BigInt(0) to numRegexes by batchSize).toVector.par
  batches.foreach { start =>
    val end = (start + batchSize - 1).min(numRegexes)
    for (i <- start to end) {
      val r = enumerate.decode(i)
      if (i % 100_000 == 0) { print("*") }
      for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "") {
        try {
          val future = Future { lex(r, s).getOrElse(Nil) }
          val markedBits = Await.result(future, 20.seconds)
          val derBits = rebit.lex(r, s.toList)
          if (markedBits != derBits) {
            println(s"\n[${i}]-\nreg: $r\nstr: $s")
            println(s"\n${pp(r)}")
            println(s"markedBits = $markedBits\nderBits   = $derBits")
            print("Type 'N' to exit, anything else to continue: ")
            val input = scala.io.StdIn.readLine()
            if (input.trim.toLowerCase == "n") {
              System.exit(1)
            }
          }
        } catch {
          case _: TimeoutException =>
            println(s"Regex: $r\nString:$s")
            System.exit(1)
        }
      }
    }
  }
  println("\nAll tests passed!")
}



/*
 Regex Examples Ordered by Size (Shortest → Longest)
Regex:
%(%(ONE ~ ZERO)) | (( "b" | ZERO ) ~ ( "a" ~ "b" ) | ( "a" | "c" | (ONE ~ ONE)) )
String: "a"
Regex:
%(%(%("c"))) | (( ("b" ~ "a") | ("c" | ONE) ) | (ZERO | "a" | (ZERO ~ ONE)) )
String: "a"
Regex:
%(%(ONE)) | (( ("a" | ZERO) ~ ("b" | ONE) ) ~ ( (ONE ~ "a") ~ ONE ))
String: "aa"
Regex:
%(%(%(ZERO))) | (( "a" ~ %("b") ) ~ ( ("a" | "c") ~ (ONE | ONE) ))
String: "aa"
*/
/*         @@case SEQ(r1, r2) =>
            val (r1ms, c1) = shift(ms, r1, consumed)
            val r2Lists = r1ms.reshuffle.map(m => shift(List(m), r2, c1))
            val r2ms = r2Lists.flatMap(_._1)
            val c2 = r2Lists.exists(_._2)
            (r2ms.reshuffle.prune, c1 || c2) */
```


#### Short summary: 

empty definition using pc, found symbol in pc: 