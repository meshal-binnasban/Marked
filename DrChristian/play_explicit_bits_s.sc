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

def mkeps3(r: Rexp) : Bits = r match {
  case ONE => List(Ep)
  case ALT(r1, r2) =>
    if (nullable(r1)) Lf :: mkeps3(r1) else Ri :: mkeps3(r2)
  case SEQ(r1, r2) =>
    mkeps3(r1) ++ mkeps3(r2)
  case STAR(r) => List(En)
  case NTIMES(r, n) => List(EnT)
}

def stripPoints(r: Rexp): Rexp = r match {
  case POINT(_, CHAR(c)) => CHAR(c)
  case SEQ(r1, r2)       => SEQ(stripPoints(r1), stripPoints(r2))
  case ALT(r1, r2)       => ALT(stripPoints(r1), stripPoints(r2))
  case STAR(r1)          => STAR(stripPoints(r1))
  case NTIMES(r1, n)     => NTIMES(stripPoints(r1), n)
  case _                 => r
}

def decode_checked(r: Rexp, bs: Bits) = decode_checked_aux(r, bs) match {
  case (v, Nil) => v
  case _ => throw new Exception("Not decodable")
}

def decode_checked_aux(r: Rexp, bs: Bits): (Val, Bits) = ((r, bs): @unchecked) match {
  case (ONE, bs) => (Empty, bs)
  case (CHAR(c), bs) => (Chr(c), bs)
  case (ALT(r1, r2), Lf :: bs) =>
    val (v, bs1) = decode_checked_aux(r1, bs)
    (Left(v), bs1)
  case (ALT(r1, r2), Ri :: bs) =>
    val (v, bs1) = decode_checked_aux(r2, bs)
    (Right(v), bs1)
  case (SEQ(r1, r2), bs) =>
    val (v1, bs1) = decode_checked_aux(r1, bs)
    val (v2, bs2) = decode_checked_aux(r2, bs1)
    (Sequ(v1, v2), bs2)
  case (STAR(r1), Nx :: bs) =>
    val (v, bs1) = decode_checked_aux(r1, bs)
    val (Stars(vs), bs2) = (decode_checked_aux(STAR(r1), bs1)  : @unchecked)
    (Stars(v::vs), bs2)
  case (STAR(_), En :: bs) => (Stars(Nil), bs)

  case (NTIMES(r1,n), NxT::bs) if n > 0 => 
    val (v, bs1) = decode_checked_aux(r1, bs)
    val (Nt(vs,ns), bs2) = (decode_checked_aux(NTIMES(r1,n-1), bs1)  : @unchecked)
    (Nt(v::vs,n), bs2)
  
  case (NTIMES(r1, n), EnT :: bs) =>
    if (n == 0 || nullable(r1)) (Nt(Nil, n), bs)
    else throw new Exception(s"$n iterations remain and $r1 is not nullable")
}

def fin(r: Rexp) : Boolean = (r: @unchecked) match 
  case ZERO => false
  case ONE => false
  case CHAR(_) => false
  case POINT(_, CHAR(_)) => true
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
  case NTIMES(r, n) => fin(r)

def mkfin3(r: Rexp): List[Bits] = r match 
  case POINT(bss, CHAR(_)) => bss
  case ALT(r1, r2) if fin(r1) && fin(r2) =>
     mkfin3(r1) ++ mkfin3(r2)
  case ALT(r1, r2) if fin(r1) => mkfin3(r1)
  case ALT(r1, r2) if fin(r2) => mkfin3(r2)
  case SEQ(r1, r2) if fin(r1) && nullable(r2) =>
    val nR1 = mkfin3(r1).map(_ ++ (mkeps3(r2))) 
    if (fin(r2)) mkfin3(r2) ++ nR1 else nR1
  case SEQ(r1, r2) =>mkfin3(r2)
  case STAR(r) => mkfin3(r) <+> En
  case NTIMES(r, n) => mkfin3(r) <+> EnT


//temp ntimes solution
def hasNTIMES(r: Rexp): Boolean = r match {
  case NTIMES(_, _)    => true
  case SEQ(r1, r2)     => hasNTIMES(r1) || hasNTIMES(r2)
  case ALT(r1, r2)     => hasNTIMES(r1) || hasNTIMES(r2)
  case STAR(r1)        => hasNTIMES(r1)
  case _               => false
}

def finFinal(r: Rexp): Boolean = 
  if (!hasNTIMES(r)) fin(r)

  else fin(r) && mkfin3(r).exists { bits =>
    try {
      decode_checked(stripPoints(r), bits)
      true
    } catch {
      case _: Exception => false
    }
}

def mkfin3Final(r: Rexp): List[Bits] = {
  val originalR = stripPoints(r)
  if (!hasNTIMES(r)) mkfin3(r)
  else 
    {
      mkfin3(r).filter { bits =>
      try {
        decode_checked(originalR, bits)
        true
      } catch {
        case _: Exception => false
      }
      }
  }
}
//end of temp ntimes solution


// shift function from the paper
def shift(m: Boolean, bs: List[Bits], r: Rexp, c: Char) : Rexp = 
  (r: @unchecked) match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if (m && d == c) POINT((bs <+> Ch) , CHAR(d)) else CHAR(d)
  case POINT(bss, CHAR(d)) => if (m && d == c) POINT((bs <+> Ch)  , CHAR(d)) else CHAR(d)
  case ALT(r1, r2) => ALT(shift(m, (bs <+> Lf)  , r1, c), shift(m, (bs <+> Ri) , r2, c))

  case SEQ(r1, r2) if m && nullable(r1) && fin(r1) => //maybe no need with mkfin ?
    SEQ(shift(m, bs , r1, c), shift(true, ((mkfin3(r1)<+> Sq) <+>Sq2) ++ ((bs <++> mkeps3(r1)) <+> Sq2), r2, c))
  case SEQ(r1, r2) if m && nullable(r1)=> 
    SEQ(shift(m, bs , r1, c), shift(true, ((bs <++> mkeps3(r1)) <+> Sq2) , r2, c))

  case SEQ(r1, r2) if fin(r1) => SEQ(shift(m, bs , r1, c), shift(true, (mkfin3(r1) <+> Sq ), r2, c))
  case SEQ(r1, r2) => SEQ(shift(m, bs , r1, c), shift(false, Nil, r2, c))

  case STAR(r) if m && fin(r)=>STAR(shift(true, (bs <+> Nx) ++ ((mkfin3(r)) <+> Nx), r, c))
  case STAR(r) if fin(r) =>STAR(shift(true,(mkfin3(r) )<+> Nx , r, c))
  case STAR(r) if m =>STAR(shift(m,bs <+> Nx , r, c))
  case STAR(r) => STAR(shift(false, Nil, r, c))

  //case NTIMES(r,n) if n <= 0 => NTIMES(r, 0) 
  case NTIMES(r,n) if m && fin(r)=>NTIMES(shift(true, ((bs <+> NxT)) ++ (mkfin3(r) <+> NxT), r, c),n-1)
  case NTIMES(r,n) if fin(r) =>NTIMES(shift(true, (mkfin3(r) <+> NxT) , r, c),n)
  case NTIMES(r,n) if m =>NTIMES(shift(m,bs <+> NxT , r, c),n)
  case NTIMES(r,n) => NTIMES(shift(false, Nil, r, c),n)
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
  then Some(if (s == Nil) (List(mkeps3(r))) else mkfin3(mat(r, s)))
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

// multiple arguments ?
import scala.math.Ordering.Implicits.seqOrdering

def revertToRawBits2(sequences: List[Bits]): List[Bits] =
  sequences.map(_.filter(bit => bit != Sq && bit != Ch && bit != Ep  && bit != Ep && bit != St && bit != Cl && bit != Sq2 ))
 
def revertToRawBits(bits: Bits): Bits =
  bits.filter(bit => bit != Sq && bit != Ch && bit != Ep  && bit != Ep && bit != St && bit != Cl && bit != Sq2)

def printHelper(sequencesOption: Option[List[Bits]], r: Rexp, derivBits:Bits, derivVal:Val) = {
  val sequences=sequencesOption.getOrElse(List())
  println("+--------Final Marked Bits and Values--------+") 
  for ((bits, i) <- sequences.zipWithIndex) {
    
    println(s"${i+1}- { ${bits.mkString(", ")} }") 
    // println(s"Value= ${dec2(br2, bits)}")
  }
  println("+--------       Reference List       --------+")
  println(s"1- { ${derivBits.mkString(", ")} }")
  println(s"Value= ${derivVal}")

  println("+--------Testing New Select Functions--------+")
  val posixList=selectPOSIX(sequences,r)
  println(s"selectPOSIX(sequences,r): ${posixList}")
  if(posixList != derivBits)
  println("************ Error, Not Matching ************")
  else
    println("Selected List Match Derivative")



/*   println(s"+--------Flatten & Value Equiality Tests--------+")
  if (selectedBits == derivBits && flatten(selectedValue) == flatten(derivVal)) {
    println("Matched Bits & Flatten Value.")
  } else {
    println(s"No Match, ${flatten(selectedValue)} != ${flatten(derivVal)}")
  } */
}

def selectPOSIX(sequences: List[Bits],r:Rexp): Bits =
  pickList(revertToRawBits2(sequences),r)

// norm comparison code
def pickList(sequences: List[Bits], r: Rexp): Bits = {
  sequences.reduceLeft { (a, b) =>
    if (compareNormPath(flattenVNWithPath2(dec2(r, a)), flattenVNWithPath2(dec2(r, b))) <= 0) a else b
  }
}

def pickList2(sequences: List[Bits], r: Rexp): Bits = {
  sequences.reduceLeft { (a, b) =>
    val pathNormA = flattenVNWithPath2(dec2(r, a))
    val pathNormB = flattenVNWithPath2(dec2(r, b))

    val comparision = compareNormPath(pathNormA, pathNormB)
    if (comparision <= 0) a else b
  }
}

import scala.math.Ordering.Implicits._

def compareNormPath(listA: List[(List[Int], Int)], listB: List[(List[Int], Int)]): Int = (listA, listB) match 
  case (Nil, Nil) => 0  // two lists are equal
  case (Nil, _)   => -1 // list a is shorter, consider it better? 
  case (_, Nil)   => 1 // list b is shorter, consider it better?                    
  case ((pathA, normA) :: restA, (pathB, normB) :: restB) =>
    if (normA != normB) {
      if (normA > normB) -1 // list a has higher norm
      else 1   // list b has higher norm
    } else {
      if (pathA < pathB) -1 //list a has smaller path, consider it better? 
      else if (pathA > pathB) 1 //list b has smaller path, consider it better? 
      else compareNormPath(restA, restB)      // 0, go to next elements
    }

def flattenVNWithPath2(v: Val, path: List[Int] = List(0)): List[(List[Int], Int)] = v match 
  case Sequ(v1, v2) =>
    (path, norm(v)) ::
      flattenVNWithPath2(v1, path :+ 1) ++
      flattenVNWithPath2(v2, path :+ 2)

  case Left(v1) =>
    (path, norm(v)) ::
      flattenVNWithPath2(v1, path :+ 1)

  case Right(v1) =>
    (path, norm(v)) ::
      flattenVNWithPath2(v1, path :+ 2)

  case Stars(vs) =>
    (path, norm(v)) :: vs.zipWithIndex.flatMap { case (vi, i) => flattenVNWithPath2(vi, path :+ (i + 1))}

  case Nt(vs, _) =>
    (path, norm(v)) ::
      vs.zipWithIndex.flatMap { case (vi, i) =>
        flattenVNWithPath2(vi, path :+ (i + 1))
      }

  case _ =>
    List((path, norm(v)))

def flattenVN(v: Val): List[Val] = v match {
/*   case Empty =>
    List(v)
  case Chr(_) =>
    List(v) */
  case Sequ(v1, v2) => v::(flattenVN(v1) ++ flattenVN(v2))
  case Left(v1) => v::flattenVN(v1)
  case Right(v1) => v::flattenVN(v1)
  case Stars(vs) => v::vs.flatMap(flattenVN)
  case Nt(vs, _) => v::vs.flatMap(flattenVN)
  case _ => List(v)
}

def norm(v: Val): Int = v match {
  case Empty => 0
  case Chr(_) => 1
  case Sequ(v1, v2) => norm(v1) + norm(v2)
  case Left(v1) => norm(v1)
  case Right(v1) => norm(v1)
  case Stars(vs) => vs.map(norm).sum
  case Nt(vs, _) => vs.map(norm).sum
}

def flattenVNWithPath(v: Val, path: String = "Λ"): List[(Val, String)] = v match {
  case Sequ(v1, v2) =>
    val p1 = s"$path.1"
    val p2 = s"$path.2"
    (v, path) :: (flattenVNWithPath(v1, p1) ++ flattenVNWithPath(v2, p2))

  case Left(v1) =>
    val p = s"$path.1"
    (v, path) :: flattenVNWithPath(v1, p)

  case Right(v1) =>
    val p = s"$path.2"
    (v, path) :: flattenVNWithPath(v1, p)

  case Stars(vs) =>
    val vspathList = vs.zipWithIndex.flatMap { case (vi, i) =>
      val p = s"$path.${i + 1}"
      flattenVNWithPath(vi, p)
    }
    (v, path) :: vspathList
  case Nt(vs, _) =>
    val vspathList = vs.zipWithIndex.flatMap { case (vi, i) =>
      val p = s"$path.${i + 1}"
      flattenVNWithPath(vi, p)
    }
    (v, path) :: vspathList
  case _ =>
    List((v, path))
}


//end of norm comparison

//("a" | "ab") ~ ("bc" | "c") // choose late S, but needs structure to know
@main
def test1() = {
  println("=====Test====")
  val br2= ("a" | "ab") ~ ("bc" | "c")
  val s = "abc".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  val markSequencesList=lex(br2, s)
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal) 

}

//%("a") ~ ("aa"|"a")  // choose late S 
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

  val markSequencesList=lex(br2, s)
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal)
}

// (ONE|"a") ~ %("a") // choose one without Sq2 as it indicates choosing empty for r1
@main
def test3() = {
  println("=====Test====")
  val br2= (ONE|"a") ~ %("a")
  val s = "aaa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  val markSequencesList=lex(br2, s)
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal)
}

//  %( "a" | "aa" ) // STAR, maybe have some count of consumption at each iteration, or add S and late is better? 
@main
def test4() = {
  println("=====Test====")
  val br2= %( "a" | "aa" )
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  val markSequencesList=lex(br2, s)
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal)
}

//Nested STAR
// %(%("a")) // STAR, maybe have some count of consumption at each iteration, or add S and late is better? 
@main
def test5() = {
  println("=====Test====")
  val br2=  %(%("a"))
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  val markSequencesList=lex(br2, s)
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal)
  
}

//%("aa") ~ %(%("a")) // STAR, maybe have some count of consumption at each iteration, or add S and late is better? also, eliminate Sq2 works
@main
def test6() = {
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

  val markSequencesList=lex(br2, s)
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal)
}

//( %("a") ~ %("a") ) ~ "a"  // STAR, maybe have some count of consumption at each iteration, or add S and late is better? also, late S and eliminate Sq2 works
@main
def test7() = {
  println("=====Test====")
  val br2= ( %("a") ~ %("a") ) ~ "a"
  val s = "aaa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  val markSequencesList=lex(br2, s)
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal)

}

//  %( %( %("a") ) )   // STAR, maybe have some count of consumption at each iteration, or add S and late is better? 
@main
def test8() = {
  println("=====Test====")
  val br2=  %( %( %("a") ) )  
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  val markSequencesList=lex(br2, s)
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal)
}

// %( "a" | "aa" ) // STAR, maybe have some count of consumption at each iteration, or add S and late is better?
@main
def test9() = {
  println("=====Test====")
  val br2=  %( "a" | "aa" )
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  val markSequencesList=lex(br2, s)
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal)

}

// %("a" ~ %("a") ) // STAR, maybe have some count of consumption at each iteration, or add S and late is better?
@main
def test10() = {
  println("=====Test====")
  val br2= %("a" ~ %("a") )
  val s = "aaa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  val markSequencesList=lex(br2, s)
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal)

}

// %( %("a") ~ "a" ) // STAR, maybe have some count of consumption at each iteration, or add S and late is better? 
@main
def test11() = {
  println("=====Test====")
  val br2=  %( %("a") ~ ("a") )
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  val markSequencesList=lex(br2, s)
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal)

}

// %("a") ~ (%("a") ~ "a" )  // choose late S, but needs structure to know, this has two bit sequences that didn't consume in r1 SEQ, but one sequence is posix and it is the one that consumes outer SEQ r1 then don't consume inner SEQ r1. logic should work of late S and avoid Sq2, if other sequence that consumes in outer r1 and inner r1 then it will be picked.
@main
def test12() = {
  println("=====Test====")
  val br2=  %("a") ~ (%("a") ~ "a" )
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  val markSequencesList=lex(br2, s)
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal)

}


// %("a") ~ (%("a") ~ "a" ) // posix will be picked by late S logic, since other bit sequence didn't consume r1 (Sq2)
@main
def test13() = {
  println("=====Test====")
  val br2= %("a") ~ %("a")
  val s = "a".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  val markSequencesList=lex(br2, s)
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal)

}

//(("a"|"b") | ("ab")) ~ (("bc") | ("c" | "b")) // choose late S, but needs structure to know
@main 
def test14() = {
  println("=====Test====")
  val br2 = (("a"|"b") | ("ab")) ~ (("bc") | ("c" | "b"))
  val s = "abc".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 
  val markSequencesList=lex(br2, s)
  val sequencesList=markSequencesList.getOrElse(List())
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal) 
}


//(%( %( ONE ~ ZERO ) ) |  (("b"|ZERO) ~"ab")  |   ( ("a"|"c")   | ("a" ~ ONE)))  // choose left in alt logic should work.  
@main 
def test15() = {
  println("=====Test====")
  val br2 = (%( %( ONE ~ ZERO ) ) |   (("b"|ZERO) ~"ab")  |   ( ("a"|"c")   | ("a" ~ ONE))    )    

  val s = "a".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 
  val markSequencesList=lex(br2, s)
  val sequencesList=markSequencesList.getOrElse(List())
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal) 
}

//(ONE | "c") ~ ("cc" | "c") // late S and avoid not consuming in r1 of SEQ (Sq2)
@main 
def test16() = {
  println("=====Test====")
  val br2 = (ONE | "c") ~ ("cc" | "c")
  val s = "cc".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 
  val markSequencesList=lex(br2, s)
  val sequencesList=markSequencesList.getOrElse(List())
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal) 
}
  //#10 in notes: (("a"|"b") | ("ab")) ~ (("bc") | ("c" | "b")) input abc
  // %("a") ~ %("a")


//(("a"|"b") | ("ab")) ~ (("bc") | ("c" | "b")) // choose late S, but needs structure to know
@main 
def test17() = {
  println("=====Test====")
  val br2 = (("a"|"b") | ("ab")) ~ (("bc") | ("c" | "b")) 
  val s = "abc".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 
  val markSequencesList=lex(br2, s)
  val sequencesList=markSequencesList.getOrElse(List())
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal) 
}

@main
def testNTIMES() = {
  println("=====Test====")
  //val br2 = SEQ( SEQ(NTIMES(ONE,2) , NTIMES("a",2)) , NTIMES(STAR("a"),2)  )  //working now
  val br2=NTIMES(STAR(NTIMES("a",1)),2) // close the ntimes list early when counting in fin

  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")

  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  val markSequencesList=lex(br2, s)
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal) 
  
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
        (2, cs => SEQ(cs(0), cs(1))),
        (1, cs => NTIMES(cs(0), 9))
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

        v1s match {
          case Some(valuesList) => 
            if (!valuesList.contains(v2)) {
              println(s"[${i}]- reg: $r str: $s")
              println(s"mark: ${lex(r, s.toList).get} bder: ${rebit.lex(r, s.toList)}")
              print("Type 'N' to exit, anything else to continue: ")
              System.exit(1) 
          }
          val valuesSet: Set[Val] = valuesList.toSet
          if (valuesList.size != valuesSet.size) {
            println("Duplicate")
            println(s"All Values : ${valuesList.mkString(", ")}")
            System.exit(1)
            }
          val flattenedValues = valuesList.map(flatten)
          flattenedValues.foreach { flatVal =>
            if (flatVal != s) {
              println("Mismatch in flatten")
              println(s"Flatten value: '$flatVal', Input string: '$s'")
              System.exit(1)
              }
          }
          case None =>
            println("fail to generate values")
            println(s"[${i}]- reg: $r str: $s")
            println(s"mark: ${lex(r, s.toList).get} bder: ${rebit.lex(r, s.toList)}")
            System.exit(1)
        } //end of match

    }// end for
  }
}
  println("\nAll tests passed!")
}

@main
def strongTestParallel() = {
  given rexp_cdata: CDATA[Rexp] = List(
    (0, _ => ONE),
    (0, _ => ZERO),
    (0, _ => CHAR('a')),
    (0, _ => CHAR('b')),
    (0, _ => CHAR('c')),
    (1, cs => STAR(cs(0))),
    (2, cs => ALT(cs(0), cs(1))),
    (2, cs => SEQ(cs(0), cs(1)))
   // (1, cs => NTIMES(cs(0), 9))
  )

  val alphabet = LazyList('a', 'b')
  val numRegexes = BigInt(10_000_000_000L)
  val batchSize = BigInt(100_000L)
  val batches = (BigInt(0) to numRegexes by batchSize).toVector.par

  batches.foreach { start =>
    val end = (start + batchSize - 1).min(numRegexes)
    for (i <- start to end) {
      val r = enumerate.decode(i)
      if (i % 100_000 == 0) print("*")

      for (s <- regenerate.generate_up_to(alphabet)(10)(r).take(9) if s.nonEmpty) {
        val markedBitsList = Try(lex(r, s.toList)).getOrElse(None)
        val v2 = rebit.lex(r, s.toList)

        markedBitsList match {
          case Some(bitsList) =>
            try{
                val selectedBits = selectPOSIX(bitsList,r)

                if (selectedBits != v2) {
                  println(s"[${i}]- reg: $r str: $s")
                  println(s"selected Bits: ${selectedBits}")
                  //println(s"Selected Value: ${selectedValue}")
                  println(s"All Bits List: ${bitsList}")
                  println(s"Derivative Bits: ${v2}")
                 // println(s"Derivative Value: ${v2}")
                  print("Type 'N' to exit, anything else to continue: ")
                  val input = scala.io.StdIn.readLine()
                  if (input.trim.toLowerCase == "n") {
                    System.exit(1)
                  }
                }
              } catch {
                  case e: Exception =>
                    println(s"Error processing regex: $r, string: $s")
                    println(s"Exception: ${e.getMessage}")
                    System.exit(1)
                    }
          case None =>
            println("fail to generate values")
            println(s"[${i}]- reg: $r str: $s")
            println(s"mark: ${lex(r, s.toList).get}")
            println(s"bder: ${rebit.lex(r, s.toList)}")
            System.exit(1)

        }//
      }
    }
  }

  println("\nAll tests passed!")
}



