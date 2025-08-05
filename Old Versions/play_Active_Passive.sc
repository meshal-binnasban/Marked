//
// Algorithm from "A Play on Regular Expressions"
//
// augmented with bitsequences



import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate//, regenerate._
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


def mkeps3(r: Rexp) : Bits = r match {
  //case APPOINT(mark, ONE) => List(Ep)
  case ONE => List(Ep)
  case ALT(r1, r2) =>
    if (nullable(r1)) Lf :: mkeps3(r1) else Ri :: mkeps3(r2)
  case SEQ(r1, r2) =>
    mkeps3(r1) ++ mkeps3(r2)
 // case STAR(r) => List(En)
  //case NTIMES(r, n) => List(EnT)
  //case INIT(r1) => mkeps3(r1) 
}

def fin(r: Rexp) : Boolean = (r: @unchecked) match 
  case ZERO => false
  case ONE => false
  case CHAR(_) => false
  case POINT(_, CHAR(_)) => true 
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
 // case STAR(r) => fin(r)
 // case NTIMES(r, n) => fin(r)
 // case INIT(r1) => fin(r1) 

def mkfin3(r: Rexp): List[Bits] = r match 
  case POINT(mark, CHAR(_)) => mark.bits
  case ALT(r1, r2) if fin(r1) && fin(r2) => mkfin3(r1) ::: mkfin3(r2)
  case ALT(r1, r2) if fin(r1) => mkfin3(r1)
  case ALT(r1, r2) if fin(r2) => mkfin3(r2)
  case SEQ(r1, r2) if fin(r1) && nullable(r2) && fin(r2) => mkfin3(r2) ::: mkfin3(r1).map(_ ++ (mkeps3(r2))) 
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin3(r1).map(_ ::: (mkeps3(r2))) 
  case SEQ(r1, r2) => mkfin3(r2)
  //case STAR(r) => (mkfin3(r) <+> En )
  //case NTIMES(r, n) => mkfin3(r) <+> EnT
  //case INIT(r1) => mkfin3(r1) 

def mkfin4(r: Rexp): List[Bits] = r match 
  case POINT(mark, CHAR(_)) =>mark.bits
  case ALT(r1, r2) if fin(r1)  => mkfin4(r1) 
  case ALT(r1, r2)  => mkfin4(r2)
  case SEQ(r1, r2) if fin(r1) && nullable(r2)  => mkfin4(r1).map(_ ++ (mkeps3(r2))) 
  case SEQ(r1, r2) => mkfin4(r2)


def finMark(m: Mark, r: Rexp): Boolean = r match {
  case ONE => m.str.isEmpty
  case CHAR(_) => m.str.isEmpty
  case ALT(r1, r2) => finMark(m, r1) || finMark(m, r2)
  case SEQ(r1, r2) =>finMark(m, r1) && finMark(m, r2)
  case _ => false
}
// add suspended marks and active, maybe APoint with true/false for active, list of bits?/ whole string
//  then at fin check if left and retrieve sequence of active over passive. 
// in shift pass around object or all things?
// check each case to pass active/passive mark
def shift(mk: Mark ,r: Rexp): (Rexp,List[(Mark, List[Char])]) = r match 
  case ZERO => (ZERO, Nil)
  case ONE => (ONE,List((mk.copy(bits=mk.bits), List() )))
  case CHAR(d) =>
    if(mk.mark && mk.str.nonEmpty && mk.str.head == d) {
      val mark=mk.copy(bits = mk.bits, str = mk.str.tail, consumed = mk.consumed :+ d)
      (POINT(mark,CHAR(d)) , List((mark, List(d))))
    }else {
      (CHAR(d), Nil)
    }
  case POINT(_,CHAR(d)) => 
    if(mk.mark && mk.str.nonEmpty && mk.str.head == d) {
      val mark=mk.copy(bits = mk.bits, str = mk.str.tail, consumed = mk.consumed :+ d)
      (POINT(mark,CHAR(d)) , List((mark, List(d))))
    }else {
      (CHAR(d), Nil)
    }
  case ALT(r1, r2) => 
    val (shiftR1,markRemainListR1)=shift(mk.copy(bits=mk.bits <+>Lf),r1)
    val (shiftR2,markRemainListR2)=shift(mk.copy(bits=mk.bits <+>Ri),r2)
    (ALT(shiftR1,shiftR2),(markRemainListR1 ++ markRemainListR2).sortBy(_._2.length) )//.distinctBy(_._2) 
  
  case SEQ(r1, r2) =>
    val (r1Shifted, r1Marks) = shift(mk, r1)
    println(s"[SEQ] r1Marks (${r1Marks.length}):")
    r1Marks.foreach { case (m, cs) =>
      println(s"  r1Mark: ${m}, consumed: ${cs.mkString}")
    }

    r1Marks.sortBy(_._2.length).reverse.headOption match {
      case Some((m1, c1)) =>
        val fin1 = fin(r1)
        val m2pre = m1.copy(mark = true)
        println(s"[SEQ] Selected best mark from r1: $m1")
        println(s"[SEQ] fin1 = $fin1")
        println(s"[SEQ] Shifting r2 with: $m2pre")

        val (r2Shifted, r2Marks) = shift(m2pre, r2)

        println(s"[SEQ] r2Marks (${r2Marks.length}):")
        r2Marks.foreach { case (m, cs) =>
          println(s"  r2Mark: ${m}, consumed: ${cs.mkString}")
        }

        val updatedMarks = r2Marks.map { case (m2, c2) =>
          val bits = if (fin1) m1.bits else m2.bits
          val updated = m2.copy(bits = bits)
          println(s"[SEQ] Final mark: $updated, total consumed: ${(c1 ++ c2).mkString}")
          (updated, c1 ++ c2)
        }

        (SEQ(r1Shifted, r2Shifted), updatedMarks.sortBy(_._2.length))

      case None =>
        println("[SEQ] No valid mark from r1 — skipping r2")
        (SEQ(r1Shifted, r2), Nil)
    }


def mat(r: Rexp, s: List[Char], prnt: Boolean = false): Rexp = {
  val initMk = Mark(mark = true, bits = List(List()), str = s, consumed= List())
  val lis = shift(initMk, r)
  println(s"Initial Regex: \n${pp(r)}\n after shifts: \n${pp(lis._1)}\n marks: ${lis._2}\n")
  lis._1
}


def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s,true))

def lex(r: Rexp, s: List[Char]): Option[List[Bits]] =
  if matcher(r, s)
  then Some(if (s == Nil) List(mkeps3(r)) else mkfin4(mat(r, s)))
  else None

def lexer(r: Rexp, s: List[Char]) : Option[List[Val]] = {
  lex(r, s).map(_.map(dec2(r, _)))
}


def rb2(r:Rexp): Rexp = r match {
  case CHAR(_) | ONE | ZERO => ZERO
  case POINT(bss, CHAR(d)) => ONE
  case ALT(r1,r2) => rb2(r1) | rb2(r2)
  case SEQ(r1,r2) =>SEQ(rb2(r1) ,erase(r2)) | rb2(r2)
  case STAR(r1) => rb2(r1) | erase(STAR(r1))
}

def erase(r: Rexp): Rexp = r match {
  case POINT(_, CHAR(c)) => CHAR(c)
  case SEQ(r1, r2)       => SEQ(erase(r1), erase(r2))
  case ALT(r1, r2)       => ALT(erase(r1), erase(r2))
  case STAR(r1)          => STAR(erase(r1))
  case NTIMES(r1, n)     => NTIMES(erase(r1), n)
  case ONE => ONE
  case _                 => r
}

def simp(r: Rexp): Rexp = r match 
  case ALT(r1,r2)=> ( simp (r1), simp (r2 )) match
    case (ZERO,r2s) => r2s
    case (r1s, ZERO) => r1s
    //case (ZERO,ZERO) => ZERO
    case (r1s,r2s) =>
     // if (r1s == r2s) r1s else ALT(r1s , r2s) // might need to return ALT of r1s,r2s even if they are equal?
     ALT(r1s,r2s)
  case SEQ(r1,r2) => ( simp (r1), simp (r2 )) match 
    case (ZERO , _) => ZERO
    case (_, ZERO ) => ZERO
    //case (ONE , r2s) => r2s
    //case (r1s , ONE) => r1s
    case (r1s , r2s) => SEQ(r1s , r2s)

  case r => r



//(ONE | "a" ) ~ ( "a" | "aa" )
@main 
def test1() = {
  println("=====Test====")
  //val br2 = (ONE | "a" ) ~ ( "a" | "aa" )
  val br2= (ONE | "a") ~ ("a" | "aa")
  val s = "a".toList
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")
  mat(br2, s,true)
  println(s"=end=")

/*   for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  }   */ 

  val markSequencesList=lex(br2, s)
  val sequencesList=markSequencesList.getOrElse(List())
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))

  println(s"sequencesList: ${sequencesList}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")  
}

//("a" | "ab") ~ ("c" | "bc")
@main
def test2() = {
  println("=====Test====")
  val br2= ("a" | "ab") ~ ("c" | "bc")
    //%("aa") | ("aa" ~ ONE)
  val s = "abc".toList
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")
  

/*   for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  }   */

  val markSequencesList=lex(br2, s)
  val sequencesList=markSequencesList.getOrElse(List())
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))

  println(s"sequencesList: ${sequencesList}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}") 
}

//(("a"|"b") | ("ab")) ~ (("bc") | ("c" | "b"))
@main
def test3() = {
  println("=====Test====")
  val br2= (("a"|"b") | ("ab")) ~ (("bc") | ("c" | "b"))
    //%("aa") | ("aa" ~ ONE)
  val s = "abc".toList
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")
  

/*   for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  }   */ 

  val markSequencesList=lex(br2, s)
  val sequencesList=markSequencesList.getOrElse(List())
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))

  println(s"sequencesList: ${sequencesList}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}") 
}

//( ONE | ( ONE | "bc" )  )  | ( "a"| ONE ) ~ ("a" | "aa")
@main
def test4() = {
  println("=====Test====")
  val br2= ( ONE | ( ONE | "bc" )  )  | ( "a"| ONE ) ~ ("a" | "aa")
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")
  

/*   for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  }   */ 

  val markSequencesList=lex(br2, s)
  val sequencesList=markSequencesList.getOrElse(List())
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))

  println(s"sequencesList: ${sequencesList}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}") 
}

@main
def test5() = {
  println("=====Test====")
  val br2= "aa" | "bb"
  val s = "bb".toList
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")
  

/*   for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  }   */ 

  val markSequencesList=lex(br2, s)
  val sequencesList=markSequencesList.getOrElse(List())
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))

  println(s"sequencesList: ${sequencesList}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}") 
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
  case POINT(mark, CHAR(c)) => s"•$c:${mark}\n" 
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



/*
def collectMarks(r: Rexp): List[Mark] = r match
  case APPOINT(mks, _) => List(mks)
  case ALT(r1, r2)     => collectMarks(r1) ++ collectMarks(r2)
  case SEQ(r1, r2)     => collectMarks(r1) ++ collectMarks(r2)
  case STAR(r)         => collectMarks(r)
  case _               => Nil 


@annotation.tailrec
def mat(r: Rexp, s: List[Char], prnt: Boolean = false): Rexp =
  val marks = collectMarks(r)

  if (marks.forall(_.str.isEmpty)) {
    if (prnt) println(s"Regex: \n${pp(r)}") else println()
    r
  } else {
    val toShift = marks.find(m => m.active && m.str.nonEmpty)
      .orElse(marks.find(_.str.nonEmpty))

    toShift match {
      case None =>
        if (prnt) println(s"Regex: \n${pp(r)}")
        r

      case Some(m) =>
        if (prnt) println(s"Regex: \n${pp(r)}")
        mat(shift(m, r), s, prnt) // recurse
    }
  }
*/

/*   case SEQ(r1, r2) =>
    val (r1Shifted, shiftR1Marks) = shift(mk, r1)
    val result = for {
      (m1, c1) <- shiftR1Marks
      fin1 = finMark(m1, r1)
      m2pre = m1.copy(mark = fin1)
      (r2Shifted, shiftR2Marks) = shift(m2pre, r2)
      (m2, c2) <- shiftR2Marks
      finalBits = if (fin1) m1.bits else m2.bits
      m2final = m2.copy(bits = finalBits)
    } yield (m2final, c1 ++ c2)
    (SEQ(r1Shifted, r2), result.sortBy(_._2.length)) */