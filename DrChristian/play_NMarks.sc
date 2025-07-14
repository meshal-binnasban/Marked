
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
    ms.map(m => m.copy(bits = m.bits :+ b, priority = m.priority))
}

def mkeps2(r: Rexp) : Bits = r match {
  case ONE => Nil//List(Ep)
  case ALT(r1, r2) =>if (nullable(r1)) Lf :: mkeps2(r1) else Ri :: mkeps2(r2)
  case SEQ(r1, r2) => mkeps2(r1) ++ mkeps2(r2)
  case STAR(r) => List(En)
}

/* def fin(r: Rexp) : Boolean = (r: @unchecked) match 
  case ZERO => false
  case ONE => false
  case CHAR(_) => false
  case POINT(_, CHAR(_)) => true
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)

def mkfin(r: Rexp): List[Bits] = r match 
  case POINT(mark, CHAR(_)) =>mark.bits
  case ALT(r1, r2) if fin(r1) && fin(r2) => mkfin(r1) ::: mkfin(r2)
  case ALT(r1, r2) if fin(r1) => mkfin(r1)
  case ALT(r1, r2) if fin(r2) => mkfin(r2)
  case SEQ(r1, r2) if fin(r1) && nullable(r2) && fin(r2) => mkfin(r2) ::: mkfin(r1).map(_ ++ (mkeps2(r2))) 
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin(r1).map(_ ::: (mkeps2(r2))) 
  case SEQ(r1, r2) => mkfin(r2)
  case STAR(r) => (mkfin2(r) <+> En ) */

type Marks = List[Mark]
def shift(ms: Marks ,r: Rexp): Marks = 
    if (ms == Nil) Nil
    else (r: @unchecked) match {
        case ZERO => Nil
        case ONE => Nil // add bits somehow?
        case CHAR(d) =>//ms.map { m => if (m.str != Nil && m.str.head == d) m.copy(str = m.str.tail) else m } 
            for (m <- ms if m.str != Nil && m.str.head == d) yield m.copy(str=m.str.tail,priority=m.priority)
        case ALT(r1, r2) =>
            val prioMs = ms.zipWithIndex.map { case (m, i) => m.copy(priority = i) }
            val shiftLeft=shift(prioMs <:+> Lf, r1)
            val shiftRight=shift(prioMs <:+> Ri, r2)
            //println(s"in ALT:\nshiftLeft= $shiftLeft\nshiftRight= $shiftRight")
            (shiftLeft ::: shiftRight).sortBy(m => (m.str.length, m.priority)).prune
        case SEQ(r1, r2) => 
            if (nullable(r1)) {
                if (nullable(r2))
                (shift(shift(ms, r1).reshuffle ::: ms, r2) ::: shift(ms, r1) ).prune
                else shift(shift(ms, r1).reshuffle ::: ms, r2).prune
            }
            else {
                if (nullable(r2))
                (shift(shift(ms, r1).reshuffle, r2) ::: shift(ms, r1)).prune
                else {
                    //println(s"SEQ\n recieved ms=$ms\n")
                    val shiftR1=shift(ms, r1).reshuffle
                    //println(s"shift r1= $shiftR1")
                    val seqMarks=shift(shiftR1, r2)
                    println(s"After shift r2=$seqMarks \n")
                    seqMarks.prune
                    }
                } 
        case STAR(r) => shift(shift(ms, r), STAR(r)) ::: ms
    }

extension (ms: List[Mark])
  def reshuffle: List[Mark] =ms.sortBy(_.str.length)//.zipWithIndex.map { case (m, idx) => m.copy(priority = idx) }
  def prune: List[Mark] =
    var seenEmpty = false
    ms.collect {
        case m if m.str != Nil => m
        case m if m.str == Nil && !seenEmpty =>
            seenEmpty = true
            m
            }

def matcher(r: Rexp, s: String) : Boolean =
  val im = Mark(bits=List(), str=s.toList, priority = 1)
  shift(List(im),r).exists(_.str == Nil)

def lex(r: Rexp, s: String): Option[Bits] =
  if matcher(r, s) then
    if s.toList == Nil then Some(mkeps2(r))
    else Some(shift(List(Mark(bits = List(), str = s.toList)), r).head.bits)
  else None

def lexMarks(r: Rexp, s: String): Option[Marks] =
  if matcher(r, s) then
    if s.toList == Nil then 
        println("Empty string, not returning marks for now, later will adjust mkeps to return the empty maybe?")
        None
    else Some(shift(List(Mark(bits = List(), str = s.toList)), r))
  else None

/* def lexer(r: Rexp, s: List[Char]) : Option[List[Val]] = {
  lex(r, s).map(_.map(dec2(r, _)))
} */


//(ONE | "a" ) ~ ( "a" | "aa" )
@main 
def test1() = {
  println("=====Test====")
  //val br2 = (ONE | "a" ) ~ ( "a" | "aa" )
  val br2= (ONE | "a") ~ ("a" | "aa")
  val s = "a"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")    
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

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")  
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

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s) 
  println(matcher(br2,s))

  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")   
}

//( ONE | ( ONE | "bc" )  )  | ( "a"| ONE ) ~ ("a" | "aa")
@main
def test4() = {
  println("=====Test====")
  val br2= ( ONE | ( ONE | "bc" )  )  | ( "a"| ONE ) ~ ("a" | "aa")
  val s = "aa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}")
  println(s"marked Bits: ${markedBits}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}")    
}

@main
def test5() = {
  println("=====Test====")
  val br2= ( ("a" | "b") ~ (ONE | "a") ) ~ "a"
  
  val s = "aa"
  println(s"Regex:\n${pp(br2)}\n")
  println(s"=string=\n$s")

  val markedBits=lex(br2, s)
  val derivBits  = rebit.lex(br2, s.toList)
  val derivVal=rebit.blexer(br2, s)

  val marks=lexMarks(br2, s)
  println(s"marks: ${marks}")
  println(s"marked Bits: ${markedBits}")
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
def strongTestNoBitsNoONENoSTARParallel() = {
  given rexp_cdata : CDATA[Rexp] = List(
        //(0, _ => ONE),
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
