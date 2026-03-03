// simple test using SETS instead of list of marks
//
// => 26 mins (for 100_000_000)
// => not much change from the list version for 10 strings
//   
// 53 mins vs 85 mins for 20 strings and 100_000_000

import scala.language.implicitConversions

import $ivy.`org.typelevel::cats-core:2.10.0`
import cats.Show
import cats.syntax.show.*

given Show[String] with
  def show(s: String): String = s""""$s""""

import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._

type Marks = Set[String]

// old shifts function
def shifts(ms: Marks, r: Rexp) : Marks = r match {
  case ZERO => Set()
  case ONE => Set()
  case CHAR(c) => for (m <- ms; if m != "" && m.head == c) yield m.tail
  case ALT(r1, r2) => shifts(ms, r1) ++ shifts(ms, r2)
  case SEQ(r1, r2) => {
    val ms1 = shifts(ms, r1)
    (nullable(r1), nullable(r2)) match {
      case (true, true) =>  shifts(ms1 ++ ms, r2) ++ ms1
      case (true, false) => shifts(ms1 ++ ms, r2)
      case (false, true) => shifts(ms1, r2) ++ ms1
      case (false, false) => shifts(ms1, r2)
    }
  }
  case STAR(r) => {
    val ms1 = shifts(ms, r)
    if (ms1 == Set()) ms1 else ms1 ++ shifts(ms1, STAR(r))
  }
  case NTIMES(r, n) =>
    if (n == 0) Set()               // or return ms?
    else if (n == 1) shifts(ms, r)
    else {
      val ms1 = shifts(ms, r)
      if (ms1 == Set()) ms1
      else
      if (nullable(r)) ms1 ++ shifts(ms1, NTIMES(r, n - 1))
      else shifts(ms1, NTIMES(r, n - 1))
    }
}

// shifts2 function 
def shifts2 (ms: Marks, r: Rexp) : Marks = r match {
  case ZERO => Set()
  case ONE => ms
  case CHAR(c) => for (m <- ms; if m != "" && m.head == c) yield m.tail
  case ALT(r1, r2) => shifts2(ms, r1) ++ shifts2(ms, r2)
  case SEQ(r1, r2) => shifts2(shifts2(ms, r1), r2)
  case STAR(r) => {
    val ms1 = shifts2(ms, r)
    val ms2 = ms1.diff(ms)
    if (ms2 == Set()) ms else ms ++ shifts2(ms2, STAR(r)) 
  }
  case NTIMES(r1, n) =>
    if (n == 0) ms 
    else shifts2(shifts2(ms, r1), NTIMES(r1, n - 1))
}


    

// the main matching function 
def mat(r: Rexp, s: String) : Marks = 
  shifts2(Set(s), r)

def matcher(r: Rexp, s: String) : Boolean = {
  if (s == "") nullable(r)
  else mat(r, s).exists(_ == "")
}



@main
def testall() = {
  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE, "ONE"),
        (0, _ => ZERO, "ZERO"),
        (0, _ => CHAR('a'), "a"),
        (0, _ => CHAR('b'), "b"),
        (0, _ => CHAR('c'), "c"),
        (1, cs => STAR(cs(0)), "STAR"),
        (2, cs => ALT(cs(0), cs(1)), "ALT"),
        (2, cs => SEQ(cs(0), cs(1)), "SEQ")
      )
  val alphabet = LazyList('a', 'b')

  for (i <- 0L to 100_000_000L) {
    val r = decode(i)
    if (i % 100_000 == 0) { print("*") }
    val r_seq = SEQ(SEQ(r, r), r)
    val r_n = NTIMES(r, 3)
    for (s <- (generate_up_to(alphabet)(20)(r_seq).take(19)) if s != "") {
      val res1 = shifts2(Set(s), r_n) 
      val res2 = if (nullable(r)) shifts(Set(s), r_seq) ++ Set(s) else shifts(Set(s), r_seq)
      if (res1 != res2) {
        println(s"$r_n and ${s.show}")
        println(s"shifts generates ${shifts(Set(s),r_seq).show}")
        println(s"shifts2 generates ${shifts2(Set(s),r_n).show}")
        System.exit(1)
      }
    }
  }
}


// for measuring time
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

def mkstar(n: Int) = STAR("a" * n)
def mkalts(n: Int) = {
  (for (i <- (1 to n).toList) yield mkstar(i)).reduceLeft(ALT.apply)
} 

@main
def test1() = {
   val reg = NTIMES(ALT(CHAR('a'),SEQ(CHAR('a'),CHAR('a'))),3)
   val s = "aaaaa"
   val res1 = shifts(Set(s), reg) 
   val res2 = shifts2(Set(s), reg) 
   println(s"reg: $reg")
   println(s"shifts  ${res1.show}")
   println(s"shifts2 ${res2.show}")
}

@main
def test2() = {
  val reg = NTIMES(("a" | ONE), 3)
  for (n <- 0 to 5) {
    val s = "a" * n 
    println(s"$n, $s: ${matcher(reg, s)}")
  }
}

@main
def test3() = {
  val reg = SEQ(STAR(CHAR('a')),CHAR('a'))
  val s = "a"
  println(s"reg: ${reg}")
  println(s"shifts2: ${shifts2(Set(s), reg).show}")
}


/* def back(r: Rexp, s: String, a: Int, b: Int, tr: Trace): (Val, Trace) =
  r match {
/*     case ZERO => throw new RuntimeException("back: ZERO")
    case ONE =>throw new RuntimeException("back: ZERO")
/*       if (a == b) (Empty, tr)
      else throw new RuntimeException(s"back: ONE a=$a b=$b") */ */

    case CHAR(c) =>
      if (b == a + 1 && a >= 0 && b <= s.length && s(a) == c) (Chr(c), tr)
      else throw new RuntimeException(s"back: CHAR($c) a=$a b=$b")

    case ALT(r1, r2) =>
      tr.t match {
        case Alt(aa, bb, 0) :: rest if aa == a && bb == b =>
          val (v, tr1) = back(r1, s, a, b, Trace(rest))
          (Left(v), tr1)
        case Alt(aa, bb, 1) :: rest if aa == a && bb == b =>
          val (v, tr1) = back(r2, s, a, b, Trace(rest))
          (Right(v), tr1)
        case _ => throw new RuntimeException(s"back: ALT a=$a b=$b")
      }

    case SEQ(r1, r2) =>
    tr.t match {
        case Seq(aa, bb, k) :: rest  =>
        val tr0 = Trace(rest)

        if (k == b) {
            val (v1, tr1) = back(r1, s, a, b, tr0)
            (Sequ(v1, mkeps(r2)), tr1)
        } else if (k == a) {
            val (v2, tr2) = back(r2, s, a, b, tr0)
            (Sequ(mkeps(r1), v2), tr2)
        } else {
            val (v2, tr2) = back(r2, s, k, b, tr0)
            val (v1, tr1) = back(r1, s, a, k, tr2)
            (Sequ(v1, v2), tr1)
        }
        case _ => throw new RuntimeException(s"back: SEQ a=$a b=$b")
    }
    case STAR(r) =>
    if (a == b) (Stars(Nil), tr)
    else {
        tr.t match {
        case Star(bb, prev) :: rest if bb == b =>
            val (v, tr1) = back(r, s, prev, b, Trace(rest))
            val (v0, tr2) = back(STAR(r), s, a, prev, tr1)
            v0 match {
            case Stars(vs) => (Stars(vs :+ v), tr2)
            case _ => throw new RuntimeException(s"back: expected Stars a=$a b=$prev")
            }
        case _ =>throw new RuntimeException(s"back: STAR a=$a b=$b")
        }
    }
    case AND(_, _) => throw new RuntimeException("back: AND not implemented in this version")
    case NTIMES(_, _) => throw new RuntimeException("back: NTIMES not implemented in this version")
  }
 */
