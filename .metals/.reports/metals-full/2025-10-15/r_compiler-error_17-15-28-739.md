file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/play_explicit_bits_s.sc
### java.lang.AssertionError: NoDenotation.owner

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/play_explicit_bits_s.sc
text:
```scala
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
/* def decode_aux(r: Rexp, bs: Bits) : (Val, Bits) = ((r, bs): @unchecked) match {
  case (ONE(bss), bs) => (Empty, bs)
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
  case (INIT(r1),bs) => decode_aux(r1, bs) 
}

def dec2(r: Rexp, bs: Bits) = decode_aux(r, bs) match {
  case (v, Nil) => v
  case _ => throw new Exception("Not decodable")
} */

def mkeps3(r: Rexp) : Bits = r match {
  case ONE => List(Ep)
  case ALT(r1, r2) =>
    if (nullable(r1)) Lf :: mkeps3(r1) else Ri :: mkeps3(r2)
  case SEQ(r1, r2) =>
    mkeps3(r1) ++ mkeps3(r2)
  case STAR(r) => List(En)
  case NTIMES(r, n) => List(EnT)
  //case INIT(r1) => mkeps3(r1) 
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
  //case (INIT(r1), bs) =>  decode_checked_aux(r1, bs) 
}

def fin(r: Rexp) : Boolean = (r: @unchecked) match 
  case ZERO => false
  case ONE(bss) => false
  case CHAR(_) => false
  case POINT(_, CHAR(_)) => true
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
  case NTIMES(r, n) => fin(r)
  //case INIT(r1) => fin(r1) 

def mkfin3(r: Rexp): List[Bits] = r match 
  case POINT(bss, CHAR(_)) => bss
  case ALT(r1, r2) if fin(r1) && fin(r2) => mkfin3(r1) ::: mkfin3(r2)
  case ALT(r1, r2) if fin(r1) => mkfin3(r1)
  case ALT(r1, r2) if fin(r2) => mkfin3(r2)
  case SEQ(r1, r2) if fin(r1) && nullable(r2) && fin(r2) => mkfin3(r2) ::: mkfin3(r1).map(_ ++ (mkeps3(r2))) 
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin3(r1).map(_ ::: (mkeps3(r2))) 
  case SEQ(r1, r2) => mkfin3(r2)
  case STAR(r) => (mkfin3(r) <+> En )
  case NTIMES(r, n) => mkfin3(r) <+> EnT
  //case INIT(r1) => mkfin3(r1) 


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
      decode_checked(erase(r), bits)
      true
    } catch {
      case _: Exception => false
    }
}

def mkfin3Final(r: Rexp): List[Bits] = {
  val originalR = erase(r)
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
  case CHAR(d) => if (m && d == c)  POINT((bs <+> Ch) , CHAR(d)) else CHAR(d)
  case POINT(bss, CHAR(d)) => if (m && d == c) POINT((bs <+> Ch)  , CHAR(d)) else CHAR(d) // <+> Cl
  case ALT(r1, r2) => ALT(shift(m, (bs <+>Lf)   , r1, c), shift(m, (bs <+> Ri)    , r2, c)) // <::> St

  case SEQ(r1, r2) if m && nullable(r1) && fin(r1) => 
    SEQ(shift(m, bs , r1, c), shift(true, ((mkfin3(r1)<+> Sq)) ++ ((bs <++> mkeps3(r1)) <+> Sq2), r2, c)) //r1:<+> St r2: <::> St
  case SEQ(r1, r2) if m && nullable(r1)=> 
    SEQ(shift(m, bs  , r1, c), shift(true, ((bs <++> mkeps3(r1)) <+> Sq2)  , r2, c)) //r1: <+> St r2: <::> St

  case SEQ(r1, r2) if fin(r1) => SEQ(shift(m, bs , r1, c), shift(true, (mkfin3(r1) <+> Sq ), r2, c)) // r1: <+> St
  case SEQ(r1, r2) => SEQ(shift(m, bs  , r1, c), shift(false, Nil, r2, c)) // r2: <+> St

  case STAR(r) if m && fin(r)=>STAR(shift(true, ((bs <+> Nx)) ++ (((mkfin3(r))  <+> Nx)) , r, c))
  case STAR(r) if fin(r) =>STAR(shift(true,(mkfin3(r) <+> Nx) , r, c))
  case STAR(r) if m =>STAR(shift(m,(bs <+> Nx) , r, c)) // <::> St
  case STAR(r) => STAR(shift(false, Nil, r, c))

  //case NTIMES(r,n) if n <= 0 => NTIMES(r, 0) 
  case NTIMES(r,n) if m && fin(r)=>NTIMES(shift(true, ((bs <+> NxT)) ++ (mkfin3(r) <+> NxT), r, c),n-1)
  case NTIMES(r,n) if fin(r) =>NTIMES(shift(true, (mkfin3(r) <+> NxT) , r, c),n)
  case NTIMES(r,n) if m =>NTIMES(shift(m,bs <+> NxT , r, c),n)
  case NTIMES(r,n) => NTIMES(shift(false, Nil, r, c),n)
  //case INIT(r1) => INIT(shift(true, bs, r1, c)) 
}

def mat(r: Rexp, s: List[Char], prnt: Boolean = false): Rexp = s match 
  case Nil => r
  case c :: cs =>
    val initialMarked = shift(true, List(List()), r, c)
    val initialDer = rebit.bder(c, rebit.intern(erase(r)))
    if (prnt)
      /* if(nullablePoint(initialMarked)) 
        println(s"Shift - $c: \n${pp(initialMarked)}\n mkepsPoint= ${mkepsPoint(initialMarked)}")
      else 
        println(s"Shift - $c: - no points: \n${pp(initialMarked)}\n") */
      matPrintHelper(c, initialMarked, initialDer) 
    val (finalMarked, finalDerivative) = cs.foldLeft((initialMarked, initialDer)) {
      case ((r, d), ch) =>
        val nextMarked = shift(false, List(List()), r, ch)
        val nextDer = rebit.bder(ch, d)
        if (prnt)
         /*  if(nullablePoint(nextMarked)) 
          println(s"Shift - $ch: \n${pp(nextMarked)}\n mkepsPoint= ${mkepsPoint(nextMarked)}")
          else 
            println(s"Shift - $ch: - no points: \n${pp(nextMarked)}\n") */
          matPrintHelper(ch, nextMarked, nextDer) 
        (nextMarked, nextDer)
    }
    if (prnt) {
      println(s"Final Marked Regex: \n${pp(finalMarked)}")
      println(s"Final Derivative: \n${rebit.pps(finalDerivative)}")
    }
    finalMarked


def pNMat(r: Rexp, s: List[Char], prnt: Boolean = false): Rexp = s match 
  case Nil => r
  case c :: cs =>
    val (finalMarked, finalDerv) = (c :: cs).foldLeft((r, rebit.intern(erase(r)))) {
      case ((previousMarked, previousDer), ch) =>
        if prnt then
          println(s"shift/der - '$ch'")
          println("-" * 80)
        val nextMarked = shift(false, List(List()), previousMarked, ch)
        val nextDer = rebit.bder(ch, previousDer)

        val dPrevious = rebit.unintern(previousDer)
        val dNext = rebit.unintern(nextDer)
        if prnt then 
          println(s"dPrevious= \n${pp(dPrevious)}\ndNext= \n${pp(dNext)}")
/*           if nullable(dNext) then
          println(s"starting derivative mkeps= ${mkeps(dNext)}\nValue of inj Derivative= ${inj(dPrevious, ch, mkeps(dNext))}") */
        
        val rbPrevious = rb2(previousMarked)
        val rbNext = rb2(nextMarked)
        if prnt then
          println(s"rbPrevious= \n${pp(rbPrevious)}\nrbNext= \n${pp(rbNext)}")
/*           if nullable(rbNext) then
          println(s"starting marked mkeps= ${mkeps(rbNext)}\nValue of inj RB Marked=${None}")  */

        (nextMarked, nextDer)
    }
    if prnt then
      println(s"Final derivative: \n${pp(rebit.unintern(finalDerv))}\n mkeps= ${mkeps(rebit.unintern(finalDerv))}")
      println(s"Final Marked: \n${pp(rb2(finalMarked))}\n mkeps= ${mkeps(rb2(finalMarked))}")
    
    finalMarked

def lexMarked(r: Rexp, s: List[Char], prnt: Boolean = false): Val = s match
  case Nil =>
    val rb2R = rb2(r)
    if nullable(rb2R) then
      val rb2RV = mkeps(rb2R)
      println(s"mkeps → ${rb2RV}")
      rb2RV
    else throw new Exception("lexing error")

  case c :: cs =>
    val shifted = shift(false, List(List()), r, c)
    if prnt then
      println(s"Shifted with '$c':\n${pp(shifted)}")
      println(s"rb2 result:\n${pp(rb2(shifted))}")
      println("-" * 80)
    val pValue = lexMarked(shifted, cs, prnt)
    val result = mInj(rb2(r), c, pValue)
    println(s"inj $c → $result")
    result

def mInj(r: Rexp, c: Char, v: Val) : Val = (r, v) match {
  case (STAR(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
  case (SEQ(r1, r2), Sequ(v1, v2)) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Left(Sequ(v1, v2))) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Right(v2)) => Sequ(mkeps(r1), inj(r2, c, v2))
  case (ALT(r1, r2), Left(v1)) => Left(inj(r1, c, v1))
  case (ALT(r1, r2), Right(v2)) => Right(inj(r2, c, v2))
  case (CHAR(d), Empty) => Chr(c)
}


def matPrintHelper(c: Char, marked: Rexp, der: rebit.ARexp): Unit =   
  val rb2Re = rb2(marked)
  val derConverted = rebit.unintern(der)
  println(s"shift — '$c'")
  println("-" * 80)
  //println("\n[Marked Regex after shift]")
  println(pp(marked))
  println("\n[RB2 Result Simplified]")
  println(pp(simp(rb2Re)))
  println("\n[Derivative Result (simplified)]")
  println(rebit.pps(rebit.bsimp(der)))
  
  println("\n[RB2 Result]")
  println(pp(rb2Re))
  println("\n[Derivative Result]")
  println(pp(derConverted))

/*   println("\n[Nullability and mkeps]")
  if (nullable(rb2Re)) {
    println("RB2 is nullable:")
    println(s"  mkeps2 (marked Bits Retrieved ) = ${mkeps2(simp(rb2Re))}")
    println(s"  mkeps  (value extracted)        = ${mkeps(simp(rb2Re))}")
  } else {
    println("RB2 is not nullable.")
  }
  if (rebit.bnullable(der)) {
    println("Derivative is nullable:")
    println(s"  bmkeps   Bits   = ${rebit.bmkeps(rebit.bsimp(der))}")
    println(s"  mkeps (value)   = ${mkeps(simp(derConverted))}")
  } else {
    println("Derivative is not nullable.")
  } */

  println("\n[Comparison]")
  val simplifiedEqual = erase(simp(derConverted)) == erase(simp(rb2Re))
  val rawEqual = erase(derConverted) == erase(rb2Re)
  println(f"  Simplified  match: ${if (simplifiedEqual) "YES" else "NO "}")
  println(f"  Raw         match: ${if (rawEqual) "YES" else "NO "}")
  println("-" * 80)



def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s,false))

def lex(r: Rexp, s: List[Char]) : Option[List[Bits]] = {
  if matcher(r, s)
  then Some(if (s == Nil) (List(mkeps3(r))) else mkfin3(mat(r, s,true)))
  else None
}

def lexer(r: Rexp, s: List[Char]) : Option[List[Val]] = {
  lex(r, s).map(_.map(dec2(r, _)))
}

def rb2(r:Rexp): Rexp = r match {
  case CHAR(_) | ONE | ZERO => ZERO
  case POINT(bss, CHAR(d)) => ONE
  case ALT(r1,r2) => rb2(r1) | rb2(r2)
  case SEQ(r1,r2) =>SEQ(rb2(r1) ,erase(r2)) | rb2(r2)
  case STAR(r1) => rb2(r1) | erase(STAR(r1))
  //case INIT(r1) => rb2(r1) 
}

def rb(r: Rexp): Set[Rexp] = r match {
  case CHAR(_) | ONE | ZERO => Set()
  case POINT(bss, CHAR(_)) => Set(ONE)
  case ALT(r1, r2) => rb(r1) | rb(r2)
  case SEQ(r1, r2) => rb(r1).map(r => SEQ(r, erase(r2))) | rb(r2)
  case STAR(r1) => rb(r1).map(r => SEQ(r, erase(STAR(r1))))
 // case INIT(r1) => rb(r1)
}

def rb3(r:Rexp,bs:Bits): (Rexp,Bits) = r match {
  case CHAR(_) | ONE | ZERO => (ZERO,bs)
  case POINT(bss, CHAR(d)) => 
    (ONE,revertToRawBits(bss.head))
  case ALT(r1, r2) =>
    (ALT(rb3(r1,bs)._1,rb3(r2,bs)._1),bs.tail)
  case SEQ(r1,r2) =>
    val (r1b, bs1)=rb3(r1,bs)
    val (r2b, bs2)=rb3(r2,bs1)
    println(s"inside seq, r=${SEQ(r1b ,r2b)}, bs1=$bs1 bs2=$bs2")

    (SEQ(r1b ,r2b),bs2)
  case STAR(r1) => 
    val (rStar, bs1) = rb3(r1, bs)
    (rStar | erase(STAR(r1)),bs1)
}

def erase(r: Rexp): Rexp = r match {
  case POINT(_, CHAR(c)) => CHAR(c)
  case SEQ(r1, r2)       => SEQ(erase(r1), erase(r2))
  case ALT(r1, r2)       => ALT(erase(r1), erase(r2))
  case STAR(r1)          => STAR(erase(r1))
  case NTIMES(r1, n)     => NTIMES(erase(r1), n)
  case ONE => ONE
  //case INIT(r1) => erase(r1)
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
  //case INIT(r1) => simp(r1) // might need to return INIT of r1
  case r => r

def mkeps2(r: Rexp) : List[Bits] = r match {
  case ONE => Nil
  case ALT(r1, r2) => 
    if (nullable(r1))  mkeps2(r1) else mkeps2(r2)  
  case SEQ(r1, r2) => mkeps2(r1) ++ mkeps2(r2)
  case STAR(r) => List(List(En)) 
  case NTIMES(r, n) => List(List(EnT)) 
  //case INIT(r1) => mkeps2(r1)
}

def mkepsPoint(r: Rexp): List[Bits] = r match
  case POINT(bss, CHAR(d)) => bss
  case ALT(r1, r2) if nullablePoint(r1) && nullablePoint(r2) =>
     mkepsPoint(r1) ::: mkepsPoint(r2)
  case ALT(r1, r2) if nullablePoint(r1) => mkepsPoint(r1)
  case ALT(r1, r2) if nullablePoint(r2) => mkepsPoint(r2)
  case SEQ(r1, r2) if nullablePoint(r1) && nullablePoint(r2) => mkepsPoint(r1) ::: mkepsPoint(r2)
  case SEQ(r1, r2) if nullablePoint(r1) => mkepsPoint(r1)
  case SEQ(r1, r2) => mkepsPoint(r2)
  case STAR(r) =>mkepsPoint(r)
  //case INIT(r1) => mkepsPoint(r1)
  case _ => Nil

def nullablePoint(r: Rexp): Boolean = r match {
  case POINT(_, _) => true
  case ALT(r1, r2) => nullablePoint(r1) || nullablePoint(r2)
  case SEQ(r1, r2) => nullablePoint(r1) || nullablePoint(r2)
  case STAR(r) => nullablePoint(r)
  case NTIMES(r, _) => nullablePoint(r)
  //case INIT(r) => nullablePoint(r)
  case _ => false
}




def pickByLatestS(bitsLists: List[Bits]): Bits = {
  bitsLists.maxBy(bits => sPosition(bits).getOrElse(-1))

}

def sPosition(bits: List[Bit]): Option[Int] = {
  var stack = List.empty[Int]
  var pairs = List.empty[(Int, Int)] 

  for ((b, i) <- bits.zipWithIndex) {
    b match {
      case St => stack = i :: stack
      case Sq | Sq2 if stack.nonEmpty =>   
        val start = stack.head
        stack = stack.tail
        pairs = (start, i) :: pairs  

      case _ => () 
    }
  }
  pairs.sortBy(_._1).headOption.map(_._2)
}

def selectPOSIX(sequences: List[Bits],r:Rexp): Bits =
  pickList(revertToRawBits2(sequences),r)

def pickList(sequences: List[Bits], r: Rexp): Bits = {
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
  case ONE => s"1 nullable  \n"
  case CHAR(c) => s"$c\n"
  case POINT(bss, CHAR(c)) => s"•$c:${bss.mkString(",")}\n" 
  case ALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => "SEQ\n" ++ pps(r1, r2)
  case STAR(r) => s"STAR\n" ++ pps(r)
  case NTIMES(r, n) => s"NTIMES($n)\n" ++ pps(r)
  //case INIT(r1) => pps(r1)
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

/* def revertToRawBits2(sequences: List[Bits]): List[Bits] =
  sequences.map(_.filter(bit => bit != Sq && bit != Ch && bit != Ep  && bit != Ep && bit != St && bit != Cl && bit != Sq2 ))
 
def revertToRawBits(bits: Bits): Bits =
  bits.filter(bit => bit != Sq && bit != Ch && bit != Ep  && bit != Ep && bit != St && bit != Cl && bit != Sq2)
 */
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


//end of norm comparison

//("a" | "ab") ~ ("bc" | "c") // choose late S, but needs structure to know
@main
def test1() = {
  println("=====Test====")
  val br2= (("a" | "ab") ~ ("c" | "bc"))
    //%("aa") | ("aa" ~ ONE)
  val s = "abc".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)
  //lexMarked(br2, s, true)

  /* for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  }   */

 
  val markSequencesList=lex(br2, s)
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))

  println(s"markSequencesList: ${markSequencesList}")
  println(s"derivBits: ${derivBits}")
  println(s"derivVal: ${derivVal}") 
  //printHelper(markSequencesList,br2, derivBits, derivVal) 

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
  val br2= ( ONE |"a") ~ %("a")
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

  val testR= (ONE ~ ("bc"| ("c"|"b")) ) | ((ONE ~ "c")| ONE)
  println(s"pp TestR=\n${pp(testR)}")
  println(s"nullable testR=${nullable(testR)}") 
  println(s"mkepsVal=${mkeps(testR)}")

/*   for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  }  */
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



@main 
def test17() = {
  println("=====Test====")
  val br2 = (ONE | "a" ) ~ ( "a" | "aa" )
  
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

/*   for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  }  */
  val markSequencesList=lex(br2, s)
  val sequencesList=markSequencesList.getOrElse(List())
  val derivBits  = rebit.lex(br2, s)
  val derivVal=rebit.blexer(br2, s.mkString(""))
  printHelper(markSequencesList,br2, derivBits, derivVal) 

}

@main 
def test18() = {
  println("=====Test====")
  val br2 = ( ONE | ( ONE | "bc" )  )  | ( "a"| ONE ) ~ ("a" | "aa")
  //(  ONE(Nil) | ( ONE(Nil) | "bc" )  )  | ( (ZERO ~ ZERO) | ("a"| ONE(Nil)) ) ~ ("a" | "aa")
     
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

/*   for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  }  */
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
       // (0, _ => ONE(Nil)),
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
      //  (0, _ => ONE(Nil)),
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
        //(0, _ => ONE(Nil)),
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
      //  (0, _ => ONE(Nil)),
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
   // (0, _ => ONE(Nil)),
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

@main
def rbDerTestParallel() = {
  given rexp_cdata: CDATA[Rexp] = List(
  //  (0, _ => ONE(Nil)),
    (0, _ => ZERO),
    (0, _ => CHAR('a')),
    (0, _ => CHAR('b')),
    (0, _ => CHAR('c')),
    //(1, cs => STAR(cs(0))),
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
        try {
          matTest(r,s.toList) 
          } catch {
            case e: Throwable => 
              println(s"mismath:\n$r\n${s}\n${e.getMessage}")
              System.exit(1)
            }
        

      }// end for s <- regenerate
    }
  }

  println("\nAll tests passed!")
}

@main
def rbDerTest() = {
  given rexp_cdata: CDATA[Rexp] = List(
 //   (0, _ => ONE(Nil)),
    (0, _ => ZERO),
    (0, _ => CHAR('a')),
    (0, _ => CHAR('b')),
    (0, _ => CHAR('c')),
    //(1, cs => STAR(cs(0))),
    (2, cs => ALT(cs(0), cs(1))),
    (2, cs => SEQ(cs(0), cs(1)))
   // (1, cs => NTIMES(cs(0), 9))
  )

  val alphabet = LazyList('a', 'b')
  var i=BigInt(0)
  val numRegexes=BigInt(10_000_000_000L)
  while(i<= numRegexes){
    val r = enumerate.decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "")
    { 
      try {
        matTest(r,s.toList) 
        } catch {
          case e: Throwable => 
            println(s"mismath:\n$r\n${s}\n${e.getMessage}")
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


def matTest(r: Rexp, s: List[Char]) : Rexp = s match{ 
  case Nil => r
  case c :: cs =>
    val initial = shift(true, List(List()), r, c)
    val initRB2Regex=rb2(initial)
    val initDerRegex = rebit.bder(c, rebit.intern(erase(r))) 
    val derRegexConverted=rebit.unintern(initDerRegex) 
    
    if( erase(simp(derRegexConverted)) !=  erase(simp(initRB2Regex))){
      println("\n*** Simplified Regexes are Not Equal ***\n")
      println(s"Simplified RB2 initial $c =\n ${pp(simp(initRB2Regex))}\n") 
      println(s"Simplified Derivatives:\n${rebit.pps(rebit.bsimp(initDerRegex))}")
      throw new Exception(s"Emismatched, r=${r}, Simplified RB2 initial $c =\n ${pp(simp(initRB2Regex))}\n,Simplified Derivatives:\n${rebit.pps(rebit.bsimp(initDerRegex))} ")
      if( erase(derRegexConverted) ==  erase(initRB2Regex))
      println("\n*** Regexes are Equal ***")
      else
        println("\n--- Regexes are Different ---")   
      }

    val (finalMarked, _ )=cs.foldLeft((initial, initDerRegex)) { case ((r, d), c) =>
      val rest=shift(false, List(List()), r, c)
      val rb2Regex=rb2(rest)
      val derRegex = rebit.bder(c, d)
      val derRegexConverted=rebit.unintern(derRegex)

      if( erase(simp(derRegexConverted)) !=  erase(simp(rb2Regex))){
        println("\n*** Simplified Regexes are Not Equal ***\n")
        println(s"Simplified RB2 initial $c =\n ${pp(simp(rb2Regex))}\n") 
        println(s"Simplified Derivatives:\n${rebit.pps(rebit.bsimp(derRegex))}")
        throw new Exception(s"Emismatched, r=${r}, Simplified RB2  $c =\n ${pp(simp(rb2Regex))}\n,Simplified Derivatives:\n${rebit.pps(rebit.bsimp(derRegex))} ")

        if( erase(derRegexConverted) ==  erase(rb2Regex))
        println("\n*** Regexes are Equal ***")
        else
          println("\n--- Regexes are Different ---")   
      }
      (rest, derRegex)
    }
    finalMarked
}
```



#### Error stacktrace:

```
dotty.tools.dotc.core.SymDenotations$NoDenotation$.owner(SymDenotations.scala:2623)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.isSelfSym(SymDenotations.scala:718)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:330)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.fold$1(Trees.scala:1665)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.apply(Trees.scala:1667)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1698)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1802)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:457)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1706)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1802)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:457)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.fold$1(Trees.scala:1665)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.apply(Trees.scala:1667)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1704)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1802)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:457)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse$$anonfun$13(ExtractSemanticDB.scala:391)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:334)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:386)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:348)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.fold$1(Trees.scala:1665)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.apply(Trees.scala:1667)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1698)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1802)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:457)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1706)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1802)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:457)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.fold$1(Trees.scala:1665)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.apply(Trees.scala:1667)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1704)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1802)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:457)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1753)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1802)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:354)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse$$anonfun$11(ExtractSemanticDB.scala:377)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:334)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:377)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1757)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1671)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1802)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:351)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse$$anonfun$1(ExtractSemanticDB.scala:315)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:334)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:315)
	dotty.tools.pc.SemanticdbTextDocumentProvider.textDocument(SemanticdbTextDocumentProvider.scala:36)
	dotty.tools.pc.ScalaPresentationCompiler.semanticdbTextDocument$$anonfun$1(ScalaPresentationCompiler.scala:242)
```
#### Short summary: 

java.lang.AssertionError: NoDenotation.owner