import $file.rexp, rexp._
import $file.ARexp, ARexp._


//import $file.Val, Val._
import scala.language.implicitConversions


// nullable function: tests whether the an (annotated) 
// regular expression can recognise the empty string
def bnullable (r: ARexp) : Boolean = r match {
  case AZERO => false
  case AONE(_) => true
  case ACHAR(_,_) => false
  case AALTS(_, rs) => rs.exists(bnullable)
  case ASEQ(_, r1, r2) => bnullable(r1) && bnullable(r2)
  case ASTAR(_, _) => true
  //case ANTIMES(_, r, n) => if (n == 0) true else bnullable(r)
}

// generates a bitsequence for how the derivative matches the
// empty string
def bmkeps(r: ARexp) : Bits = r match {
  case AONE(bs) => bs
  case AALTS(bs, r::Nil) => bmkeps(r) ++ bs
  case AALTS(bs, r::rs) => 
    if (bnullable(r)) bmkeps(r) ++ bs else bmkeps(AALTS(bs, rs))  
  case ASEQ(bs, r1, r2) => bmkeps(r2) ++ bmkeps(r1) ++ bs
  case ASTAR(bs, r) => S :: bs
  //case ANTIMES(bs, r, 0) => S :: bs
  //case ANTIMES(bs, r, n) => bmkeps(ANTIMES(Nil, r, n - 1)) ++ bmkeps(r) ++ List(Z) ++ bs
}

// derivative of a regular expression w.r.t. a character
def bder(c: Char, r: ARexp) : ARexp = r match {
  case AZERO => AZERO
  case AONE(_) => AZERO
  case ACHAR(bs, d) => if (c == d) AONE(bs) else AZERO
  case AALTS(bs, rs) => AALTS(bs, rs.map(bder(c, _)))
  case ASEQ(bs, r1, r2) => 
    if (bnullable(r1)) AALT(bs, ASEQ(Nil, bder(c, r1), r2), fuse(bmkeps(r1), bder(c, r2)))
    else ASEQ(bs, bder(c, r1), r2)
  case ASTAR(bs, r) => ASEQ(Z :: bs, bder(c, r), ASTAR(Nil, r))
  //case ANTIMES(bs, r, n) => 
  //  if (n == 0) AZERO else ASEQ(Z :: bs, bder(c, r), ANTIMES(Nil, r, n - 1))
}

// derivative w.r.t. a string (iterates bder)
def bders (r: ARexp, s: List[Char]) : ARexp = s match {
  case Nil => r
  case c::s => bders(bder(c, r), s)
}

// unsimplified lexing function (produces a value without simplification)
def blex(r: ARexp, s: List[Char]) : Bits = s match {
  case Nil => if (bnullable(r)) bmkeps(r) else throw new Exception("Not matched")
  case c::cs => blex(bder(c, r), cs)
}

def blexer(r: Rexp, s: String) : Val = 
  decode(r, blex(internalise(r), s.toList))


//=======================
// simplification 


def flts(rs: List[ARexp]) : List[ARexp] = rs match {
  case Nil => Nil
  case AZERO :: rs => flts(rs)
  case AALTS(bs, rs1) :: rs => rs1.map(fuse(bs, _)) ++ flts(rs)
  case r1 :: rs => r1 :: flts(rs)
}


def distinctWith[B](xs: List[B], 
                    eq: (B, B) => Boolean, 
                    acc: List[B] = Nil): List[B] = xs match {
  case Nil => Nil
  case x::xs => {
    if (acc.exists(eq(_, x))) distinctWith(xs, eq, acc)
    else x::distinctWith(xs, eq, x::acc)
  }
} 


// equivalence
def eqm(r1: ARexp, r2: ARexp) : Boolean = (r1, r2) match {
  case (AZERO, AZERO) => true
  case (AONE(_), AONE(_)) => true
  case (ACHAR(_, c), ACHAR(_, d)) => c == d
  case (ASEQ(_, ra1, ra2), ASEQ(_, rb1, rb2)) => eqm(ra1, rb1) && eqm(ra2, rb2)
  case (AALTS(_, Nil), AALTS(_, Nil)) => true
  case (AALTS(_, r1::rs1), AALTS(_, r2::rs2)) => eqm(r1, r2) && eqm(AALTS(Nil, rs1), AALTS(Nil, rs2))
  case (ASTAR(_, r1), ASTAR(_, r2)) => eqm(r1, r2)
  //case (ANTIMES(_, r1, n1), ANTIMES(_, r2, n2)) => n1 == n2 && eqm(r1, r2)
  case _ => false 
}

def bsimp(r: ARexp): ARexp = r match {
  case ASEQ(bs1, r1, r2) => (bsimp(r1), bsimp(r2)) match {
      case (AZERO, _) => AZERO
      case (_, AZERO) => AZERO
      case (AONE(bs2), r2s) => fuse(bs2 ++ bs1, r2s)
      // destroys posix property
      //case (AALTS(bs2, rs), r2s) => AALTS(bs1 ::: bs2, rs.map(ASEQ(Nil, _, r2s)))
      case (r1s, r2s) => ASEQ(bs1, r1s, r2s)
  }
  case AALTS(bs1, rs) => 
    distinctWith[ARexp](flts(rs.map(bsimp)), 
                        (r1: ARexp, r2: ARexp) => eqm(r1, r2) ) match {  
      case Nil => AZERO
      case r::Nil => fuse(bs1, r)
      case rs => AALTS(bs1, rs)
  }
  //case ANTIMES(bs, _, 0) => AONE(bs)
  case r => r
}

def bders_simp(r: ARexp, s: List[Char]) : ARexp = s match {
  case Nil => r
  case c::cs => bders_simp(bsimp(bder(c, r)), cs)
}


def blex_simp(r: ARexp, s: List[Char]) : Bits = s match {
  case Nil => if (bnullable(r)) bmkeps(r) 
              else throw new Exception("Not matched")
  case c::cs => blex_simp(bsimp(bder(c, r)), cs)
}

def blexer_simp(r: Rexp, s: String) : Val = 
  decode(r, blex_simp(internalise(r), s.toList).reverse)



@main
def test1() = {
  println("=====Test====")
  val r = ("a" | "ab") ~ ("c" | "bc")
  val s = "abc"
  println("=string=")
  println(s)
  println(time_needed(1000, blexer_simp(r,s)))
  

}

@main
def testExample() = {
  val r = %( %("a") | %("aa") | %("aaa") | %("aaaa") | %("aaaaa") ) 

  for (i <- 0 to 10_000 by 100) {
    val s = "a" * i  //+ "b"         
    println(s"i= $i  bsimp= ${time_needed(10, blexer_simp(r,s)) }")
  }

}
