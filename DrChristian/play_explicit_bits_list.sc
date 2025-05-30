//
// Algorithm from "A Play on Regular Expressions"
//
// augmented with bitsequences



import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate//, regenerate._
import $file.rebit


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
  /*case (STAR(r1), bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    val (Stars(vs), bs2) = (decode_aux(STAR(r1), bs1)  : @unchecked)
    (Stars(v::vs), bs2)
  }*/
}

def dec2(r: Rexp, bs: Bits) = decode_aux(r, bs) match {
  case (v, Nil) => v
  case _ => throw new Exception("Not decodable")
}

def mkeps(r: Rexp) : Bits = r match {
  case ONE => Nil
  case POINT(bss, CHAR(_)) => bss.head
  case ALT(r1, r2) => 
    if (nullable(r1)) Lf :: mkeps(r1) else Ri :: mkeps(r2)  
  case SEQ(r1, r2) => mkeps(r1) ++ mkeps(r2)
  case STAR(r) => List(En)
}

def mkeps3(r: Rexp) : List[Bits] = r match {
  case ONE => List(Nil)
  case POINT(bss, CHAR(_)) => bss
  case ALT(r1, r2) => 
    if (nullable(r1)) mkeps3(r1).map(Lf :: _) else mkeps3(r2).map(Ri :: _)  
  case SEQ(r1, r2) => 
    for {
      b1 <- mkeps3(r1)
      b2 <- mkeps3(r2)
    } yield b1 ++ b2
  case STAR(r) => List(List(En))
}

// fin function from the paper
// checks whether a mark is in "final" position
def fin(r: Rexp) : Boolean = (r: @unchecked) match {
  case ZERO => false
  case ONE => false
  case CHAR(_) => false
  case POINT(_, CHAR(_)) => true
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
}

/* def mkfin(r: Rexp) : Bits = r match {
  case POINT(bss, CHAR(_)) => bss.head
  case ALT(r1, r2) => if (fin(r1)) mkfin(r1) else mkfin(r2)  
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin(r1) ++ mkeps(r2)
  case SEQ(r1, r2) => mkfin(r2)
  case STAR(r) => mkfin(r) ++ List(En)
} */

/* def mkfin2(r: Rexp) : Set[Bits] = r match {
  case POINT(bss, CHAR(_)) => Set(bss.head)
  case ALT(r1, r2) if fin(r1) && fin(r2) => mkfin2(r1) | mkfin2(r2)
  case ALT(r1, r2) if fin(r1) => mkfin2(r1)
  case ALT(r1, r2) if fin(r2) => mkfin2(r2) 

  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin2(r1).map(_ ++ mkeps(r2)) //| (if (fin(r2)) mkfin2(r2) else Set.empty[Bits])
  case SEQ(r1, r2) => mkfin2(r2)
  case STAR(r) => mkfin2(r).map(_ ++ List(En))
} */

def mkfin3(r: Rexp): List[Bits] = r match 
  case POINT(bss, CHAR(_)) => bss
  case ALT(r1, r2) if fin(r1) && fin(r2) => mkfin3(r1) ++ mkfin3(r2)
  case ALT(r1, r2) if fin(r1) => mkfin3(r1)
  case ALT(r1, r2) if fin(r2) => mkfin3(r2)
  case SEQ(r1, r2) if fin(r1) && nullable(r2) =>
    val nR1 = for {
      b1 <- mkfin3(r1)
      b2 <- mkeps3(r2)
    } yield b1 ++ b2
   // nR1
    if (fin(r2)) mkfin3(r2) ++ nR1 else nR1 
  case SEQ(r1, r2) =>mkfin3(r2)
  case STAR(r) => mkfin3(r).map(_ :+ En)


/* def mkfin4(r: Rexp): List[Bits] = r match 
  case POINT(bss, CHAR(_)) => bss
  case ALT(r1, r2) if fin(r1) => mkfin4(r1)
  case ALT(r1, r2) if fin(r2) => mkfin4(r2)
  case SEQ(r1, r2) if fin(r1) && nullable(r2) =>
    val nR1 = for {
      b1 <- mkfin4(r1)
      b2 <- mkeps3(r2)
    } yield b1 ++ b2
   // nR1
    if (fin(r2)) mkfin4(r2) ++ nR1 else nR1 
  case SEQ(r1, r2) =>mkfin4(r2)
  case STAR(r) => mkfin4(r).map(_ :+ En) */
 

// shift function from the paper
def shift(m: Boolean, bs: List[Bits], r: Rexp, c: Char) : Rexp = 
  (r: @unchecked) match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if (m && d == c) POINT(bs, CHAR(d)) else CHAR(d)
  case POINT(bss, CHAR(d)) => if (m && d == c) POINT(bs, CHAR(d)) else CHAR(d)
  case ALT(r1, r2) => ALT(shift(m, bs.map(bits => bits :+ Lf), r1, c), shift(m, bs.map(bits => bits :+ Ri), r2, c))
  case SEQ(r1, r2) if m && nullable(r1) =>
    if(fin(r1))
    SEQ(shift(m, bs, r1, c), shift(true, mkfin3(r1) ++ (for {b <- bs;s <- mkeps3(r1)} yield b ++ s), r2, c))
    else
    SEQ(shift(m, bs, r1, c), shift(true, for {b <- bs;s <- mkeps3(r1)} yield b ++ s, r2, c))
  case SEQ(r1, r2) if fin(r1) => SEQ(shift(m, bs, r1, c), shift(true, mkfin3(r1), r2, c))
  case SEQ(r1, r2) => SEQ(shift(m, bs, r1, c), shift(false, Nil, r2, c))

  case STAR(r) if m && fin(r)=>STAR(shift(true,bs.map(_ ++ List(Nx)) ++ mkfin3(r).map(_ ++ List(Nx)), r, c))
  case STAR(r) if fin(r) =>STAR(shift(true,mkfin3(r).map(_ ++ List(Nx)) , r, c))
  case STAR(r) if m =>STAR(shift(m,bs.map(_ ++ List(Nx)) , r, c))
  case STAR(r) => STAR(shift(false, Nil, r, c))
}

// the main matching function (by using BINIT only in 
// the first step a mark is shifted into the Rexp)
def mat(r: Rexp, s: List[Char]) : Rexp = s match {
  case Nil => r
  case c::cs => cs.foldLeft(shift(true, List(List()), r, c))((r, c) => shift(false, List(List()), r, c))
}

def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s))

def lex(r: Rexp, s: List[Char]) : Option[List[Bits]] = {
  if matcher(r, s)
  then Some(if (s == Nil) (mkeps3(r)) else mkfin3(mat(r, s)))
  else None
}

def lexer(r: Rexp, s: List[Char]) : Option[List[Val]] = {
  lex(r, s).map(_.map(dec2(r, _)))
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
  case ONE => "1\n"
  case CHAR(c) => s"$c\n"
  case POINT(bss, CHAR(c)) => s"•$c:${bss.mkString(",")}\n" 
  case ALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => "SEQ\n" ++ pps(r1, r2)
  case STAR(r) => s"STAR\n" ++ pps(r)
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
   //case Rec(_, v) => flatten(v)
 }

//("a" | "ab") ~ ("bc" | "c")
@main
def test1() = {
  println("=====Test====")
  //(a + ab)(bc + c)
  val br2 = ("a" | "ab") ~ ("bc" | "c")
  val s = "abc".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 
  println(s"=final list=")
  val sequencesList=lex(br2, s)
  sequencesList.foreach {
    list => list.foreach(bits => println(s" $bits"))
    }
  
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
  println(rebit.blexer(br2, s.mkString("")))

  
  println("Final Marked Values for testing")
  sequencesList.foreach {
    list => list.foreach(bits => 
      val v= dec2(br2, bits)
      println(s"Value=${v}")
      val vString=flatten(v)
      println(s"Input string == flatten is: ${vString == s.mkString("")}")
    )
    }
}

//%("a") ~ ("aa"|"a") 
@main
def test2() = {
  println("=====Test====")

  val br2= %("a") ~ ("aa"|"a")
  val s = "aaaaa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 
  println(s"=final list=")
  val sequencesList=lex(br2, s)
  sequencesList.foreach {
    list => list.foreach(bits => println(s" $bits"))
    }
  
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
  println(rebit.blexer(br2, s.mkString("")))

  
  println("Final Marked Values for testing")
  sequencesList.foreach {
    list => list.foreach(bits => println(dec2(br2, bits)))
    }


}

//(ONE  |  %("c"|"d")) 
@main
def test3() = {
  println("=====Test====")
  val br2 = (ONE  |  %("c"|"d"))
  //val br2= STAR( STAR("a") )
  val s = "ccc".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 
  println(s"=final list=")
  val sequencesList=lex(br2, s)
  sequencesList.foreach {
    list => list.foreach(bits => println(s" $bits"))
    }
  
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
  println(rebit.blexer(br2, s.mkString("")))

  
  println("Final Marked Values for testing")
  sequencesList.foreach {
    list => list.foreach(bits => println(dec2(br2, bits)))
    }


}

// (ONE|"a") ~ %("a") 
@main
def test4() = {
  println("=====Test====")
  //val br2 =STAR( STAR("a") )
 // val br2=SEQ(STAR(CHAR('a')),STAR(CHAR('a')))
  //val br2=SEQ(ALT(ONE,CHAR('a')),STAR(CHAR('a')))
  val br2= (ONE|"a") ~ %("a")
  val s = "aaa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 
  println(s"=final list=")
  val sequencesList=lex(br2, s)
  sequencesList.foreach {
    list => list.foreach(bits => println(s" $bits"))
    }
  
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
  println(rebit.blexer(br2, s.mkString("")))

  
  println("Final Marked Values for testing")
  sequencesList.foreach {
    list => list.foreach(bits => println(dec2(br2, bits)))
    }

}

// ONE |  %( "a" | "aa" ) 
@main
def test5() = {
  println("=====Test====")
  val br2= ONE |  %( "a" | "aa" )
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 
  println(s"=final list=")
  val sequencesList=lex(br2, s)
  sequencesList.foreach {
    list => list.foreach(bits => println(s" $bits"))
    }
  
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
  println(rebit.blexer(br2, s.mkString("")))

  
  println("Final Marked Values for testing")
  sequencesList.foreach {
    list => list.foreach(bits => println(dec2(br2, bits)))
    }
}

//Nested STAR
//ONE | %(%("a")) 
@main
def test6() = {
  println("=====Test====")
  val br2= ONE | %(%("a"))
  val s = "aaa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 
  println(s"=final list=")
  val sequencesList=lex(br2, s)
  sequencesList.foreach {
    list => list.foreach(bits => println(s" $bits"))
    }
  
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
  println(rebit.blexer(br2, s.mkString("")))

  
  println("Final Marked Values for testing")
  sequencesList.foreach {
    list => list.foreach(bits => println(dec2(br2, bits)))
    }

}

//%("aa") ~ %(%("a")) - doesn't work in this version
@main
def test7() = {
  println("=====Test====")
  val br2= %("aa") ~ %(%("a"))
  val s = "aaa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  val sequencesList=lex(br2, s)
  sequencesList.foreach {
    list => list.foreach(bits => println(s" $bits"))
    }
  
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
  println(rebit.blexer(br2, s.mkString("")))

  
  println("Final Marked Values for testing")
  sequencesList.foreach {
    list => list.foreach(bits => println(dec2(br2, bits)))
    }
  
  //println(lexer(br2,s)) */
}

//( %("a") ~ %("a") ) ~ "a" - works now - check more input chars
@main
def test8() = {
  println("=====Test====")
  val br2= ( %("a") ~ %("a") ) ~ "a"
  //val br2=ALT(ONE,STAR(ALT(CHAR('a'),SEQ(CHAR('a'),CHAR('a')))))
  val s = "aaa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  val sequencesList=lex(br2, s)
  sequencesList.foreach {
    list => list.foreach(bits => println(s" $bits"))
    }
  
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
  println(rebit.blexer(br2, s.mkString("")))

  
  println("Final Marked Values for testing")
  sequencesList.foreach {
    list => list.foreach(bits => println(dec2(br2, bits)))
    }


}

// ONE | %( %( %("a") ) ) , input aa - doesn't work in this version
@main
def test9() = {
  println("=====Test====")
  val br2= ONE | %( %( %("a") ) )  
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  val sequencesList=lex(br2, s)
  sequencesList.foreach {
    list => list.foreach(bits => println(s" $bits"))
    }
  
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
  println(rebit.blexer(br2, s.mkString("")))

  
  println("Final Marked Values for testing")
  sequencesList.foreach {
    list => list.foreach(bits => println(dec2(br2, bits)))
    }

}


// ONE | %( "a" | "aa" ) , input aaa - doesn't work in this version
@main
def test10() = {
  println("=====Test====")
  val br2= ONE | %( "a" | "aa" )
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  val sequencesList=lex(br2, s)
  sequencesList.foreach {
    list => list.foreach(bits => println(s" $bits"))
    }
  
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
  println(rebit.blexer(br2, s.mkString("")))

  
  println("Final Marked Values for testing")
  sequencesList.foreach {
    list => list.foreach(bits => println(dec2(br2, bits)))
    }
}

// %("a" ~ %("a") ) , input aa -  doesn't work 
@main
def test11() = {
  println("=====Test====")
  val br2= %("a" ~ %("a") )
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  val sequencesList=lex(br2, s)
  sequencesList.foreach {
    list => list.foreach(bits => println(s" $bits"))
    }
  
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
  println(rebit.blexer(br2, s.mkString("")))

  
  println("Final Marked Values for testing")
  sequencesList.foreach {
    list => list.foreach(bits => println(dec2(br2, bits)))
    }
}

// %( %("a") ~ "a" ) , input aa -  doesn't work 
@main
def test12() = {
  println("=====Test====")
  val br2=  %( %("a") ~ ("a") )
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val reg =mat(br2, s.take(i + 1))
  println(pp(reg))
  println(s"Reg=${reg}")
  } 

  println(s"=final list=")
  val sequencesList=lex(br2, s)
  sequencesList.foreach {
    list => list.foreach(bits => println(s" $bits"))
    }
  
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
  println(rebit.blexer(br2, s.mkString("")))

  
  println("Final Marked Values for testing")
  sequencesList.foreach {
    list => list.foreach(bits => println(dec2(br2, bits)))
    }
}

// %("a") ~ (%("a") ~ "a" ) , input aa -  doesn't work 
@main
def test13() = {
  println("=====Test====")
  val br2=  %("a") ~ (%("a") ~ "a" )
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val reg =mat(br2, s.take(i + 1))
  println(pp(reg))
  println(s"Reg=${reg}")
  } 

  println(s"=final list=")
  val sequencesList=lex(br2, s)
  sequencesList.foreach {
    list => list.foreach(bits => println(s" $bits"))
    }
  
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
  println(rebit.blexer(br2, s.mkString("")))

  
  println("Final Marked Values for testing")
  sequencesList.foreach {
    list => list.foreach(bits => println(dec2(br2, bits)))
    }
}


// %("a") ~ (%("a") ~ "a" ) , input aa -  doesn't work 
@main
def test14() = {
  println("=====Test====")
  val br2= %("a") ~ %("a")
  val s = "a".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val reg =mat(br2, s.take(i + 1))
  println(pp(reg))
  println(s"Reg=${reg}")
  } 

  println(s"=final list=")
  val sequencesList=lex(br2, s)
  sequencesList.foreach {
    list => list.foreach(bits => println(s" $bits"))
    }
  
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
  println(rebit.blexer(br2, s.mkString("")))

  
  println("Final Marked Values for testing")
  sequencesList.foreach {
    list => list.foreach(bits => println(dec2(br2, bits)))
    }
}

import scala.util._
@main
def weakTest() = {
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
  var i=BigInt(0)
  val numRegexes=BigInt(10_000_000_000L)
  while(i<= numRegexes){
    val r = enumerate.decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "")
    { val v1s = Try(lex(r, s.toList)).getOrElse(None)
      val v2 = rebit.lex(r, s.toList)
        if (v1s.isDefined && !v1s.get.contains(v2)) {

          println(s"[${i}]- reg: $r str: $s")
          println(s"mark: ${lex(r, s.toList).get} bder: ${rebit.lex(r, s.toList)}")
          
          print("Type 'N' to exit, anything else to continue: ")
          val input = scala.io.StdIn.readLine()
          if (input.trim.toLowerCase == "n") {
            System.exit(1)
          }
           

        }
      }
      i+=1
  }//end whild
  println("\nAll tests passed!")
}

@main
def weakTestDecode() = {
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
  var i=BigInt(0)
  val numRegexes=BigInt(10_000_000_000L)
  while(i<= numRegexes){
    val r = enumerate.decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "")
    { val v1s = Try(lexer(r, s.toList)).getOrElse(None)
      val v2 = rebit.blexer(r, s)
        if (v1s.isDefined && !v1s.get.contains(v2)) {

          println(s"[${i}]- reg: $r str: $s")
          println(s"mark: ${lex(r, s.toList).get} bder: ${rebit.lex(r, s.toList)}")
          
          print("Type 'N' to exit, anything else to continue: ")
          val input = scala.io.StdIn.readLine()
          if (input.trim.toLowerCase == "n") {
            System.exit(1)
          }
           

        }
      }
      i+=1
  }//end whild
  println("\nAll tests passed!")
}

import scala.collection.parallel.CollectionConverters._

@main
def weakTestParallel() = {
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
        val v1s = Try(lexer(r, s.toList)).getOrElse(None)
        val v2 = rebit.blexer(r, s)
        if (v1s.isDefined && !v1s.get.contains(v2)) {
          println(s"[${i}]- reg: $r str: $s")
          println(s"mark: ${lex(r, s.toList).get} bder: ${rebit.lex(r, s.toList)}")
          print("Type 'N' to exit, anything else to continue: ")
          val input = scala.io.StdIn.readLine()
          if (input.trim.toLowerCase == "n") {
            System.exit(1)
          }
        }
      }
    }
  }
  println("\nAll tests passed!")
}

@main
def flattenWeakTestParallel() = {
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
       val v1s = Try(lexer(r, s.toList)).getOrElse(None)
       val v2 = rebit.blexer(r, s)
       if (v1s.isDefined) {
        val valuesList: List[Val] = v1s.get
        val valuesSet: Set[Val] = valuesList.toSet
        //test for duplicates 
        if (valuesList.size != valuesSet.size) {
          println("Duplicate")
          println(s"All Values : ${valuesList.mkString(", ")}")
          System.exit(1)
          }
        //test for flattened values matching input string
        val flattenedValues = valuesList.map(flatten)
        flattenedValues.foreach { flatVal =>if (flatVal != s) {
          println("Mismatch in flatten")
          println(s"Flatten value: '$flatVal', Input string: '$s'")
          System.exit(1)
          }
        }

        // test if derivative's POSIX value is in the list generated by new code  
        if (!valuesList.contains(v2)) {
          println(s"Derivative value not found in lexer output!")
          println(s"v2: $v2")
          System.exit(1)
          } 
          }

      }// end for
    }
  }
  println("\nAll tests passed!")
}

@main
def flattenWeakTest() = {
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
  var i=BigInt(0)
  val numRegexes=BigInt(10_000_000_000L)
  while(i<= numRegexes){
    val r = enumerate.decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "")
    { 
       val v1s = Try(lexer(r, s.toList)).getOrElse(None)
       val v2 = rebit.blexer(r, s)
       if (v1s.isDefined) {
        val valuesList: List[Val] = v1s.get
        val valuesSet: Set[Val] = valuesList.toSet
        //test for duplicates 
        if (valuesList.size != valuesSet.size) {
          println("Duplicate")
          println(s"All Values : ${valuesList.mkString(", ")}")
          System.exit(1)
          }
        //test for flattened values matching input string
        val flattenedValues = valuesList.map(flatten)
        flattenedValues.foreach { flatVal =>if (flatVal != s) {
          println("Mismatch in flatten")
          println(s"Flatten value: '$flatVal', Input string: '$s'")
          System.exit(1)
          }
        }

        // test if derivative's POSIX value is in the list generated by new code  
        if (!valuesList.contains(v2)) {
          println(s"Derivative value not found in lexer output!")
          println(s"v2: $v2")
          System.exit(1)
          } 
          }

      }
      i+=1
  }//end whild
  println("\nAll tests passed!")
}



