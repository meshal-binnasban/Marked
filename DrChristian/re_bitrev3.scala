//| scalaVersion: 3.8.3
//| scalacOptions: ["-deprecation", "-feature"]
//| mvnDeps:
//|   - org.scala-lang:scala3-library_3:3.8.3
//| moduleDeps:
//|   - rexp.scala

import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.annotation.tailrec

import Rexp._
import Val._
import Bit._
import ARexp._

enum ARexp {
  case AZERO
  case AONE(bs: Bits)
  case ACHAR(bs: Bits, c: Char)
  case AALTS(bs: Bits, rs: List[ARexp])
  case ASEQ(bs: Bits, r1: ARexp, r2: ARexp)
  case ASTAR(bs: Bits, r: ARexp)
  case AOPTIONAL(bs: Bits, r: ARexp)
  case ANOT(bs: Bits, r: ARexp)
  case ANTIMES(bs: Bits, r: ARexp, n: Int)
}

def AALT(bs: Bits, r1: ARexp, r2: ARexp) =
  AALTS(bs, List(r1, r2))

def erase(r: ARexp): Rexp = r match {
  case AZERO => ZERO
  case AONE(_) => ONE
  case ACHAR(_, c) => CHAR(c)
  case AALTS(_, Nil) => ZERO
  case AALTS(_, r :: Nil) => erase(r)
  case AALTS(bs, r :: rs) => ALT(erase(r), erase(AALTS(bs, rs)))
  case ASEQ(_, r1, r2) => SEQ(erase(r1), erase(r2))
  case ASTAR(_, ASTAR(_, r)) => STAR(erase(r))
  case ASTAR(_, r) => STAR(erase(r))
  case AOPTIONAL(_, r) => OPTIONAL(erase(r))
  case ANOT(_, r) => NOT(erase(r))
  case ANTIMES(_, r, n) => NTIMES(erase(r), n)
}

def fuse(bs: Bits, r: ARexp): ARexp = r match {
  case AZERO => AZERO
  case AONE(cs) => AONE(cs ++ bs)
  case ACHAR(cs, c) => ACHAR(cs ++ bs, c)
  case AALTS(cs, rs) => AALTS(cs ++ bs, rs)
  case ASEQ(cs, r1, r2) => ASEQ(cs ++ bs, r1, r2)
  case ASTAR(cs, r) => ASTAR(cs ++ bs, r)
  case AOPTIONAL(cs, r) => AOPTIONAL(cs ++ bs, r)
  case ANOT(cs, r) => ANOT(cs ++ bs, r)
  case ANTIMES(cs, r, n) => ANTIMES(cs ++ bs, r, n)
}

def internalise(r: Rexp): ARexp = r match {
  case ZERO => AZERO
  case ONE => AONE(Nil)
  case CHAR(c) => ACHAR(Nil, c)
  case ALT(r1, r2) => AALT(Nil, fuse(List(Z), internalise(r1)), fuse(List(S), internalise(r2)))
  case SEQ(r1, r2) => ASEQ(Nil, internalise(r1), internalise(r2))
  case STAR(r) => ASTAR(Nil, internalise(r))
  case OPTIONAL(r) => AOPTIONAL(Nil, internalise(r))
  case NOT(r) => ANOT(Nil, internalise(r))
  case NTIMES(r, n) => ANTIMES(Nil, internalise(r), n)
}

def code(v: Val): Bits = v match {
  case Empty => Nil
  case Chr(_) => Nil
  case Left(v) => Z :: code(v)
  case Right(v) => S :: code(v)
  case Sequ(v1, v2) => code(v1) ::: code(v2)
  case Stars(Nil) => List(S)
  case Stars(v :: vs) => Z :: code(v) ::: code(Stars(vs))
  //case NotV(v) => code(v)
  case NotV(_) => Nil
  case Nt(vs, _) => code(Stars(vs))
}

def bnullable(r: ARexp): Boolean = r match {
  case AZERO => false
  case AONE(_) => true
  case ACHAR(_, _) => false
  case AALTS(_, rs) => rs.exists(bnullable)
  case ASEQ(_, r1, r2) => bnullable(r1) && bnullable(r2)
  case ASTAR(_, _) => true
  case AOPTIONAL(_, _) => true
  case ANOT(_, r) => !bnullable(r)
  case ANTIMES(_, r, n) => if (n == 0) true else bnullable(r)
}

def bmkeps(r: ARexp): Bits = r match {
  case AONE(bs) => bs
  case AALTS(bs, r :: Nil) => bmkeps(r) ++ bs
  case AALTS(bs, r :: rs) =>
    if (bnullable(r)) bmkeps(r) ++ bs else bmkeps(AALTS(bs, rs))
  case ASEQ(bs, r1, r2) => bmkeps(r2) ++ bmkeps(r1) ++ bs
  case ASTAR(bs, _) => S :: bs
  case AOPTIONAL(bs, _) => Z :: bs
  case ANOT(bs, _) => bs
  case ANTIMES(bs, _, 0) => S :: bs
  case ANTIMES(bs, r, n) => bmkeps(ANTIMES(Nil, r, n - 1)) ++ bmkeps(r) ++ List(Z) ++ bs
}

def bder(c: Char, r: ARexp): ARexp = r match {
  case AZERO => AZERO
  case AONE(_) => AZERO
  case ACHAR(bs, d) => if (c == d) AONE(bs) else AZERO
  case AALTS(bs, rs) => AALTS(bs, rs.map(bder(c, _)))
  case ASEQ(bs, r1, r2) =>
    if (bnullable(r1)) AALT(bs, ASEQ(Nil, bder(c, r1), r2), fuse(bmkeps(r1), bder(c, r2)))
    else ASEQ(bs, bder(c, r1), r2)
  case ASTAR(bs, r) => ASEQ(Z :: bs, bder(c, r), ASTAR(Nil, r))
  case AOPTIONAL(bs, r) => fuse(S :: bs, bder(c, r))
  case ANOT(bs, r) => ANOT(bs, bder(c, r))
  case ANTIMES(bs, r, n) =>
    if (n == 0) AZERO else ASEQ(Z :: bs, bder(c, r), ANTIMES(Nil, r, n - 1))
}

@tailrec
def bders(r: ARexp, s: List[Char]): ARexp = s match {
  case Nil => r
  case c :: cs => bders(bder(c, r), cs)
}

def blex(r: ARexp, s: List[Char]): Bits = s match {
  case Nil => if (bnullable(r)) bmkeps(r) else throw new Exception("Not matched")
  case c :: cs => blex(bder(c, r), cs)
}

def blexer(r: Rexp, s: String): Val =
  decode(r, blex(internalise(r), s.toList))

def derMatcher(r: Rexp, s: String): Boolean =
  bnullable(bders_simp(internalise(r), s.toList))
  
def flts(rs: List[ARexp]): List[ARexp] = rs match {
  case Nil => Nil
  case AZERO :: rs => flts(rs)
  case AALTS(bs, rs1) :: rs => rs1.map(fuse(bs, _)) ++ flts(rs)
  case r :: rs => r :: flts(rs)
}

def distinctWith[B](xs: List[B], eq: (B, B) => Boolean, acc: List[B] = Nil): List[B] = xs match {
  case Nil => Nil
  case x :: xs =>
    if (acc.exists(eq(_, x))) distinctWith(xs, eq, acc)
    else x :: distinctWith(xs, eq, x :: acc)
}

def eqm(r1: ARexp, r2: ARexp): Boolean = (r1, r2) match {
  case (AZERO, AZERO) => true
  case (AONE(_), AONE(_)) => true
  case (ACHAR(_, c), ACHAR(_, d)) => c == d
  case (ASEQ(_, ra1, ra2), ASEQ(_, rb1, rb2)) => eqm(ra1, rb1) && eqm(ra2, rb2)
  case (AALTS(_, Nil), AALTS(_, Nil)) => true
  case (AALTS(_, r1 :: rs1), AALTS(_, r2 :: rs2)) => eqm(r1, r2) && eqm(AALTS(Nil, rs1), AALTS(Nil, rs2))
  case (ASTAR(_, r1), ASTAR(_, r2)) => eqm(r1, r2)
  case (AOPTIONAL(_, r1), AOPTIONAL(_, r2)) => eqm(r1, r2)
  case (ANOT(_, r1), ANOT(_, r2)) => eqm(r1, r2)
  case (ANTIMES(_, r1, n1), ANTIMES(_, r2, n2)) => n1 == n2 && eqm(r1, r2)
  case _ => false
}

def bsimp(r: ARexp): ARexp = r match {
  case ASEQ(bs, r1, r2) =>
    (bsimp(r1), bsimp(r2)) match {
      case (AZERO, _) => AZERO
      case (_, AZERO) => AZERO
      case (AONE(bs2), r2s) => fuse(bs2 ++ bs, r2s)
      case (r1s, r2s) => ASEQ(bs, r1s, r2s)
    }

  case AALTS(bs, rs) =>
    distinctWith[ARexp](flts(rs.map(bsimp)), (r1: ARexp, r2: ARexp) => eqm(r1, r2)) match {
      case Nil => AZERO
      case r :: Nil => fuse(bs, r)
      case rs => AALTS(bs, rs)
    }

  case AOPTIONAL(bs, r) =>
    bsimp(AALT(bs, fuse(List(Z), AONE(Nil)), fuse(List(S), bsimp(r))))

  case ANOT(bs, r) =>
    ANOT(bs, bsimp(r))

  case r => r
}

def bders_simp(r: ARexp, s: List[Char]): ARexp = s match {
  case Nil => r
  case c :: cs => bders_simp(bsimp(bder(c, r)), cs)
}

def blex_simp(r: ARexp, s: List[Char]): Bits = s match {
  case Nil =>
    if (bnullable(r)) bmkeps(r)
    else throw new Exception("Not matched")
  case c :: cs =>
    blex_simp(bsimp(bder(c, r)), cs)
}

def blexer_simp(r: Rexp, s: String): Val =
  decode(r, blex_simp(internalise(r), s.toList).reverse)

def flatten(v: Val): String = v match {
  case Empty => ""
  case Chr(c) => c.toString
  case Left(v) => flatten(v)
  case Right(v) => flatten(v)
  case Sequ(v1, v2) => flatten(v1) + flatten(v2)
  case Stars(vs) => vs.map(flatten).mkString
  //case NotV(v) => flatten(v)
  case NotV(_) => ""
  case Nt(vs, _) => vs.map(flatten).mkString
}

def size(r: Rexp): Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_) => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r) => 1 + size(r)
  case OPTIONAL(r) => 1 + size(r)
  case NOT(r) => 1 + size(r)
  case NTIMES(r, _) => 1 + size(r)
}

def asize(r: ARexp): Int = size(erase(r))

@tailrec
def cnt(xs: List[String], acc: List[(String, Int)] = Nil): List[(String, Int)] =
  (xs, acc) match {
    case (Nil, acc) => acc.reverse
    case (x :: xs, Nil) => cnt(xs, (x, 1) :: Nil)
    case (x :: xs, (y, n) :: ys) =>
      if (x == y) cnt(xs, (y, n + 1) :: ys)
      else cnt(xs, (x, 1) :: (y, n) :: ys)
  }

def p_pair(x_n: (String, Int)) =
  if (x_n._2 == 1) x_n._1 else s"${x_n._1} x ${x_n._2}"

def pretty_list(ss: List[String]): String = ss match {
  case Nil => ""
  case s :: Nil => s
  case s :: ss => s"$s, ${pretty_list(ss)}"
}

def pretty(v: Val): String = v match {
  case Empty => "Empty"
  case Chr(c) => s"$c"
  case Sequ(v1, v2) => s"Sequ(${pretty(v1)},${pretty(v2)})"
  case Left(v) => s"Left(${pretty(v)})"
  case Right(v) => s"Right(${pretty(v)})"
  case Stars(vs) =>
    val vss = vs.map(pretty)
    val css = cnt(vss)
    val vss2 = css.map(p_pair)
    s"Stars(${pretty_list(vss2)})"
  case NotV(r) => s"NotV($r)"
  case Nt(vs, n) =>
    val vss = vs.map(pretty)
    val css = cnt(vss)
    val vss2 = css.map(p_pair)
    s"Nt(${pretty_list(vss2)},$n)"
}

def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start) / (n * 1.0e9)
}