error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/play_NMarks_Exp3.sc:getOrElse.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/play_NMarks_Exp3.sc
empty definition using pc, found symbol in pc: getOrElse.
empty definition using semanticdb

found definition using fallback; symbol getOrElse
offset: 4020
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/play_NMarks_Exp3.sc
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
extension (m: Mark) {
  def <:+>(b: Bit): Mark = 
     m.copy(bits = m.bits :+ b)
  
}

def mkeps2(r: Rexp) : Bits = r match {
  case ONE => Nil//List(Ep)
  case ALT(r1, r2) =>if (nullable(r1)) Lf :: mkeps2(r1) else Ri :: mkeps2(r2)
  case SEQ(r1, r2) => mkeps2(r1) ++ mkeps2(r2)
  case STAR(r) => List(En)
}

type Marks = List[Mark]
def shifts(ms: Marks, r: Rexp): Marks = 
    //println(s"${pp(r)}\nms=$ms")
    if (ms == Nil) Nil
    else (r: @unchecked) match {
        case ZERO => Nil
        case ONE => Nil
        case CHAR(d) =>
          for (m <- ms if m.str != Nil && m.str.head == d) yield m.copy(str = m.str.tail)
        case ALT(r1, r2) =>
          ((shifts(ms <:+> Lf, r1) ) ::: (shifts(ms <:+> Ri, r2)) )
        case SEQ(r1,r2) => {
          val ms1 = shifts(ms, r1).reshuffle
          (nullable(r1), nullable(r2)) match {
            case (true, true) =>(ms1 <::+>mkeps2(r2)) ::: ms1.flatMap( m => shifts(List(m), r2)) ::: shifts(ms <::+> mkeps2(r1) ,r2)
            case (true, false) => (ms1).flatMap( m => shifts(List(m), r2)) ::: shifts((ms<::+> mkeps2(r1)),r2)
            case (false, true) => (ms1 <::+>mkeps2(r2)) ::: ms1.flatMap( m => shifts(List(m), r2))   
            case (false, false) => ms1.flatMap( m => shifts(List(m), r2))
        }
      }
        case STAR(r) =>
            val ms1 = shifts(ms<:+>Nx, r).reshuffle
            ( (ms1 <:+> En) ::: ms1.flatMap( m=> shifts(List(m), STAR(r)))   )
            //(  (ms1 <:+> En) ::: shifts(ms1, STAR(r))  ) 

        }

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
  def collectEmpty: List[Mark] =
    ms.filter(m => m.str == Nil)

def matcher(r: Rexp, s: String) : Boolean =
  val im = Mark(bits = List(), str = s.toList)
  val marks = shifts(List(im), r)
/*   println(s"-------------End of 1 matcher call-----------\n")
  marks.foreach(m => println(s"-$m") )
  println("------------------------") */  
  marks.exists(_.str == Nil)

def lex(r: Rexp, s: String): Option[Bits] =
  if matcher(r, s) then
    if s.toList == Nil then Some(mkeps2(r))
    else Some(shifts(List(Mark(bits = List(), str = s.toList)), r).collectEmpty.head.bits)
  else None

def lexM(r: Rexp, s: String): Option[Marks] =
  if matcher(r, s) then
    if s.toList == Nil then None
    else Some(shifts(List(Mark(bits = List(), str = s.toList)), r))
  else None

def lexer(r: Rexp, s: String) : Option[Val] = {
  lex(r, s).map(dec2(r, _))
} 

def lexerMarks(r: Rexp, s: String) : Option[List[Val]] = {
  lexM(r, s).map(_.map(m => dec2(r, m.bits)))
}

// %( "a" | "aa" )
@main 
def test1() = {
  println("=====Test====")
  val br2= %(%("aa"))
  val s = "aa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched") 
  
  lexerMarks(br2,s).get@@OrElse(Nil).foreach(v => println(s"-$v") )
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
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
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
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
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

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
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

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
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

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
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

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
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

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
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
  val s = "a" * 10
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}
// %(ONE | "a")
@main 
def test10() = {
  println("=====Test====")
  val br2= %((ONE | "a") | "aa")
    //%(ONE | "a")
  val s = "a" * 3
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
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

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
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

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
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

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
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

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

//( ("a" | "b") ~ (ONE | "a") ) ~ "a"
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

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

//( (ONE|"c") | %(ONE) ) ~ ("b") 
@main
def test16() = {
  println("=====Test====")
  val br2= ( (ONE|"c") | %(ONE) ) ~ ("b") 
  val s = "b"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

//( %("a" ~ ONE) )
@main
def test17() = {
  println("=====Test====")
  val br2= ( %("a" ~ ONE) )
  val s = "aa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

//(%(ONE)| %("a"| %("b")))
@main
def test18() = {
  println("=====Test====")
  val br2= ( %("a"| %("b")))
  val s = "ba"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

//( %( %("a") | "ab"))
@main
def test19() = {
  println("=====Test====")
  val br2= ( %( %("a") | "ab"))
  val s = "aabaab"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

// %( "bb" | "b" )
@main
def test20() = {
  println("=====Test====")
  val br2= %( "bb" | "b" )
  val s = "bbb"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

// ( "a" ~ %("b") ) ~ ( ("b"|ONE) ~ "a" )
@main
def test21() = {
  println("=====Test====")
  val br2= ( "a" ~ %("b") ) ~ ( ("b"|ONE) ~ "a" )
  val s = "aba" // or abb?
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
  if(markedBits.getOrElse(Nil) == derivBits) 
  println("Matched Derivative")
  else
    println("Mismatched")
}

// %( %("a"|"c") ) | ("c" | ("a" | ONE) ) ~ ("ab" | "b" )  
@main
def test22() = {
  println("=====Test====")
  val br2=  ("c" | ("a" | ONE) ) ~ ("ab" | "b" )  
  val s = "ab" // or abb?
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}\n------------------------")
  println(s"derivVal: ${derivVal}")

  println("------------------------")
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
  case (STAR(_), En::bs) => (Stars(Nil), bs)

  case (STAR(r1), Nx::bs) => {
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
def testAll() = {
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

@main
def testExamples(): Unit = {
  val tests = List(
    ("test1", %(%("aa")), "aa"),
    ("test2", %("a"), "aaa"),
    ("test3", %("aa") ~ %(%("a")), "aaaaaa"),
    ("test4", (%("a") ~ %("a")) ~ "a", "aaa"),
    ("test5", %(%(%("a"))), "aa"),
    ("test6", %("a" ~ %("a")), "aaa"),
    ("test7", %(%("a") ~ "a"), "aaa"),
    ("test8", %("a") ~ (%("a") ~ "a"), "aaa"),
    ("test9", %("a") ~ %("a"), "a" * 10),
    ("test10", %((ONE | "a") | "aa"), "aaa"),
    ("test11", (ONE | "a") ~ ("a" | "aa"), "aaa"),
    ("test12", ("a" | "ab") ~ ("c" | "bc"), "abc"),
    ("test13", (("a" | "b") | "ab") ~ ("bc" | "c" | "b"), "abc"),
    ("test14", (ONE | (ONE | "bc")) | (("a" | ONE) ~ ("a" | "aa")), "aa"),
    ("test15", (("a" | "b") ~ (ONE | "a")) ~ "a", "aa"),
    ("test16", ((ONE | "c") | %(ONE)) ~ "b", "b"),
    ("test17", %("a" ~ ONE), "aa"),
    ("test18", %("a" | %("b")), "ba"),
    ("test19", %(%("a") | "ab"), "aabaab"),
    ("test20", %("bb" | "b"), "bbb"),
    ("test21", ("a" ~ %("b")) ~ (("b" | ONE) ~ "a"), "aba"),
    ("test22", ("c" | ("a" | ONE)) ~ ("ab" | "b"), "ab")
  )

  var passed = 0
  var failed = 0

  tests.zipWithIndex.foreach {
    case ((label, r, s), i) =>
      val marked = lex(r, s).getOrElse(Nil)
      val deriv  = rebit.lex(r, s.toList)

      if (marked != deriv) {
        failed += 1
        
        println(s"\n\u001b[31mTest ${i + 1} FAILED: $label")
        println(s"Regex:\n${pp(r)}")
        println(s"Input string: $s")
        println(s"Marked bits    : $marked")
        println(s"Derivative bits: $deriv\u001b[0m")
        println("--------------------------------------------------")
      } else {
        passed += 1
        println(s"Test ${i + 1} passed: $label")
      }
  }

  println(s"\n==== Test Summary ====")
  println(s"Passed: $passed")
  println(s"Failed: $failed")
}
```


#### Short summary: 

empty definition using pc, found symbol in pc: getOrElse.