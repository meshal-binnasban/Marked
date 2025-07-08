
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


def mkeps2(r: Rexp) : Bits = r match {
  case ONE => List(Ep)
  case ALT(r1, r2) =>
    if (nullable(r1)) Lf :: mkeps2(r1) else Ri :: mkeps2(r2)
  case SEQ(r1, r2) =>
    mkeps2(r1) ++ mkeps2(r2)

}

def fin(r: Rexp) : Boolean = (r: @unchecked) match 
  case ZERO => false
  case ONE => false
  case CHAR(_) => false
  case POINT(_, CHAR(_)) => true 
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)

def mkfin(r: Rexp): List[Bits] = r match 
  case POINT(mark, CHAR(_)) =>mark.bits
  case ALT(r1, r2) if fin(r1)  => mkfin(r1) 
  case ALT(r1, r2)  => mkfin(r2)
  case SEQ(r1, r2) if fin(r1) && nullable(r2)  => mkfin(r1).map(_ ++ (mkeps2(r2))) 
  case SEQ(r1, r2) => mkfin(r2)

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
    (ALT(shiftR1,shiftR2),(markRemainListR1 ++ markRemainListR2))//.sortBy(_._2.length) )//.distinctBy(_._2) 
  
  case SEQ(r1, r2) =>
    val (r1Shifted, r1Marks) = shift(mk, r1)
    println(s"${pp(r1Shifted)}\nr1Marks: \n${r1Marks.sortBy(_._2.length).reverse}\n")
    r1Marks.sortBy(_._2.length).reverse.headOption match {
      case Some((m1, c1)) =>
        val fin1 = fin(r1)
        val m2pre = m1.copy(mark = true)
        val (r2Shifted, r2Marks) = shift(m2pre, r2)
        println(s"${pp(r2Shifted)}\nr2Marks: \n${r2Marks.sortBy(_._2.length).reverse}\n")
        val updatedMarks = r2Marks.map { case (m2, c2) =>
          val bits = if (fin1) m1.bits else m2.bits
          val updated = m2.copy(bits = bits)
          (updated, c1 ++ c2)
        }

        (SEQ(r1Shifted, r2Shifted), updatedMarks.sortBy(_._2.length))

      case None =>
        (SEQ(r1Shifted, r2), Nil)
    }


def mat(r: Rexp, s: List[Char], prnt: Boolean = false): Rexp = {
  val initMk = Mark(mark = true, bits = List(List()), str = s, consumed= List())
  val lis = shift(initMk, r)
  lis._1
}


def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s,true))

def lex(r: Rexp, s: List[Char]): Option[List[Bits]] =
  if matcher(r, s)
  then Some(if (s == Nil) List(mkeps2(r)) else mkfin(mat(r, s)))
  else None

def lexer(r: Rexp, s: List[Char]) : Option[List[Val]] = {
  lex(r, s).map(_.map(dec2(r, _)))
}


def rb2(r:Rexp): Rexp = r match {
  case CHAR(_) | ONE | ZERO => ZERO
  case POINT(mark, CHAR(d)) => ONE
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
  case ONE               => ONE
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
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")
  mat(br2, s,true)
  println(s"=end=")

/*   for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  }   */ 

/*   val markSequencesList=lex(br2, s)
  val sequencesList=markSequencesList.getOrElse(List())
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))

  println(s"sequencesList: ${sequencesList}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")   */
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

/* def mkfin2(r: Rexp): List[Bits] = r match 
  case POINT(mark, CHAR(_)) => mark.bits
  case ALT(r1, r2) if fin(r1) && fin(r2) => mkfin2(r1) ::: mkfin2(r2)
  case ALT(r1, r2) if fin(r1) => mkfin2(r1)
  case ALT(r1, r2) if fin(r2) => mkfin2(r2)
  case SEQ(r1, r2) if fin(r1) && nullable(r2) && fin(r2) => mkfin2(r2) ::: mkfin2(r1).map(_ ++ (mkeps2(r2))) 
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin2(r1).map(_ ::: (mkeps2(r2))) 
  case SEQ(r1, r2) => mkfin2(r2)
  //case STAR(r) => (mkfin2(r) <+> En )
  //case NTIMES(r, n) => mkfin2(r) <+> EnT
  //case INIT(r1) => mkfin2(r1)  */