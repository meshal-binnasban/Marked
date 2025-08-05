import $file.rexp, rexp._, rexp.Rexp._, rexp.VALUE._

abstract class ARexp
case class AZERO() extends ARexp
case class AONE(bs: Bits) extends ARexp
case class ACHAR(bs: Bits, c: Char) extends ARexp
case class AALTS(bs: Bits, rs: List[ARexp]) extends ARexp
case class ASEQ(bs: Bits, r1: ARexp, r2: ARexp) extends ARexp
case class ASTAR(bs: Bits, r: ARexp) extends ARexp
//case class ANTIMES(bs: Bits, r: ARexp, n: Int) extends ARexp


def fuse(bs: Bits, r: ARexp): ARexp = r match {
  case AZERO() => AZERO()
  case AONE(bs1) => AONE(bs ++ bs1)
  case ACHAR(bs1, c) => ACHAR(bs ++ bs1, c)
  case AALTS(bs1, rs) => AALTS(bs ++ bs1, rs)
  case ASEQ(bs1, r1, r2) => ASEQ(bs ++ bs1, r1, r2)
  case ASTAR(bs1, r) => ASTAR(bs ++ bs1, r)
}
def internalise(r: Rexp): ARexp = r match {
  case ZERO => AZERO()
  case ONE => AONE(Nil)
  case CHAR(c) => ACHAR(Nil, c)
  case ALT(r1, r2) => AALTS(Nil, List(
    fuse(List(Z), internalise(r1)),
    fuse(List(S), internalise(r2))
  ))
  case SEQ(r1, r2) =>
    val ir1 = internalise(r1)
    val ir2 = internalise(r2)
    ASEQ(Nil, ir1, fuse(bmkeps(ir1), ir2))
  case STAR(r) =>
    ASTAR(Nil, internalise(r))
  case NTIMES(r, 0) => AONE(Nil)
  case NTIMES(r, n) =>
    ASEQ(Nil, internalise(r), internalise(NTIMES(r, n - 1)))
}

import scala.language.implicitConversions

def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
// strings are coerced into Rexps
given Conversion[String, Rexp] = s => charlist2rexp(s.toList)

//val ABCD : Rexp = "abcd"

extension (r: Rexp) {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

  
// nullable function: tests whether the an (annotated) 
// regular expression can recognise the empty string
def bnullable (r: ARexp) : Boolean = r match {
  case AZERO() => false
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
  case AALTS(bs, r::Nil) => bs ++ bmkeps(r) 
  case AALTS(bs, r::rs) => 
    if (bnullable(r)) bs ++ bmkeps(r) else bmkeps(AALTS(bs, rs))  
  case ASEQ(bs, r1, r2) => bs ++ bmkeps(r1) ++ bmkeps(r2)
  case ASTAR(bs, r) => bs ++ List(S)
  //case ANTIMES(bs, r, 0) => bs ++ List(S)
  //case ANTIMES(bs, r, n) => bs ++ List(Z) ++ bmkeps(r) ++ bmkeps(ANTIMES(Nil, r, n - 1))
}

// derivative of a regular expression w.r.t. a character
def bder(c: Char, r: ARexp) : ARexp = r match {
  case AZERO() => AZERO()
  case AONE(_) => AZERO()
  case ACHAR(bs, d) => if (c == d) AONE(bs) else AZERO()
  case AALTS(bs, rs) => AALTS(bs, rs.map(bder(c, _)))
  case ASEQ(bs, r1, r2) => 
    if (bnullable(r1)) AALTS(bs, List(ASEQ(Nil, bder(c, r1), r2), fuse(bmkeps(r1), bder(c, r2))))
    else ASEQ(bs, bder(c, r1), r2)
  case ASTAR(bs, r) => ASEQ(bs ++ List(Z), bder(c, r), ASTAR(Nil, r))
  //case ANTIMES(bs, r, n) => 
  //  if (n == 0) AZERO else ASEQ(bs ++ List(Z), bder(c, r), ANTIMES(Nil, r, n - 1))
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

/* def blexer(r: Rexp, s: String) : Val = 
  decode(r, blex(internalise(r), s.toList)) */



//=======================
// simplification 


def flts(rs: List[ARexp]) : List[ARexp] = rs match {
  case Nil => Nil
  case AZERO() :: rs => flts(rs)
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
  case (AZERO(), AZERO()) => true
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
      case (AZERO(), _) => AZERO()
      case (_, AZERO()) => AZERO()
      case (AONE(bs2), r2s) => fuse(bs1 ++ bs2, r2s)
      // desroys POSIX property
      //case (AALTS(bs2, rs), r2s) => AALTS(bs1 ::: bs2, rs.map(ASEQ(Nil, _, r2s)))
      case (r1s, r2s) => ASEQ(bs1, r1s, r2s)
  }
  case AALTS(bs1, rs) => 
    distinctWith[ARexp](flts(rs.map(bsimp)), 
                        (r1: ARexp, r2: ARexp) => eqm(r1, r2) ) match {  
      case Nil => AZERO()
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
  case c::cs =>  blex_simp(bsimp(bder(c, r)), cs)
  }


/* def blexer_simp(r: Rexp, s: String) : Val = 
  decode(r, blex_simp(internalise(r), s.toList)) */


@main
def test1() = {
  val regex=internalise(("b"|"a")| ("a"~"cc"))
  //blex_simp(regex,"acc".toList)


}

