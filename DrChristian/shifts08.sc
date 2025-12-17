// simple test using SETS instead of list of marks
//
// => 26 mins (for 100_000_000)
// => not much change from the list version for 10 strings
//   
// 53 mins vs 85 mins for 20 strings and 100_000_000

import scala.language.implicitConversions
/* //> using file rexp.sc 
//> using file enumerate.sc
//> using file regenerate.sc */
import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit

// Mark now also carries where the last advancement happened:
// - (n, m): as before, overall slice for the path
// - (advStart, advEnd): exact 1-char slice consumed by the last CHAR step
case class Mark(
  n: Int,
  m: Int,
  advStart: Int = -1,
  advEnd: Int = -1
)

type Marks = Set[Mark]

var splits: Map[Int, Set[Int]] = Map.empty

def recordSplit(id: Int, k: Int): Unit =
  splits = splits.updated(id, splits.getOrElse(id, Set.empty) + k)

// shifts function 
def shifts(ms: Marks, s: String, r: Rexp): Marks = r match {
  case ZERO => Set()
  case ONE  => Set()

  case CHAR(c) =>
    // record both the advanced mark and the exact slice [m, m+1) that was consumed
    (for {
      Mark(n, m, _, _) <- ms
      if m < s.length && s(m) == c
    } yield Mark(n, m + 1, m, m + 1)).toSet

  case ALT(r1, r2) =>
    //shifts(ms, s, r1) ++ shifts(ms, s, r2)
    (shifts(ms, s, r1) ++ shifts(ms, s, r2)).groupBy(m => (m.n, m.m))
  .values.map(_.reduce(posixPrune)).toSet

  case SEQS(r1, r2, id) =>
    val ms1  = shifts(ms, s, r1)
    val inR2 = if (nullable(r1)) ms1 ++ ms else ms1
    inR2.foreach(q => recordSplit(id, q.m))

    (nullable(r1), nullable(r2)) match {
      case (true,  true)  => shifts(ms1 ++ ms, s, r2) ++ ms1
      case (true,  false) => shifts(ms1 ++ ms, s, r2)
      case (false, true)  => ms1 ++ shifts(ms1, s, r2)
      case (false, false) => shifts(ms1, s, r2)
    }

  case STARSS(r, id) =>
    val ms1 = shifts(ms, s, r)
    ms1.foreach(q => recordSplit(id, q.m))

    if (ms1.isEmpty) Set()
    else ms1 ++ shifts(ms1, s, STARSS(r, id))

  case NTIMES(r, n) =>
    if (n == 0) Set()
    else if (n == 1) shifts(ms, s, r)
    else {
      val ms1 = shifts(ms, s, r)
      if (ms1 == Set()) Set()
      else if (nullable(r)) ms1 ++ shifts(ms1, s, NTIMES(r, n - 1))
      else                  shifts(ms1, s, NTIMES(r, n - 1))
    }
}

def posixPrune(a: Mark, b: Mark): Mark =
  if (a.m != b.m) if (a.m > b.m) a else b
  else a 




// the main matching function 
def mat(r: Rexp, s: String): Marks = {
  splits = Map.empty
  shifts(Set(Mark(0, 0)), s, r)
}

def matcher(r: Rexp, s: String): Boolean = {
  if (s == "") nullable(r)
  else {
    val ms = mat(r, s)
    ms.exists(_.m == s.length)
  }
}

def lexer(r: Rexp, s: String): Val = {
  if (s == "") {
    if (nullable(r)) Empty else Invalid
  } else {
    val ms     = mat(r, s)
    val finals = ms.filter(_.m == s.length)
    if (finals.isEmpty) Invalid
    else {
      val best: Mark = finals.head
      // println(splits)
      val (v, p0) = back(r, s, best, splits = splits)
      if (isInvalid(p0)) Invalid else v
    }
  }
}

def isInvalid(q: Mark): Boolean = q.n < 0

val invalid = (Invalid, Mark(-1, -1))

def back(r: Rexp, s: String, p: Mark, splits: Map[Int, Set[Int]]): (Val, Mark) =
  // println(s"splits=$splits")
  r match {

    case ONE =>
      val Mark(n, m, _, _) = p
      if (n == m) (Empty, p) else invalid

    case CHAR(c) =>
      val Mark(n, m, _, _) = p
      if (m == n + 1 && m <= s.length && s(n) == c) (Chr(c), Mark(n, m - 1))
      else invalid

    case ALT(r1, r2) =>
      // println(s"ALT: p=$p")
      val (vL, pL) = back(r1, s, p, splits)
      val (vR, pR) = back(r2, s, p, splits)
      // println(s"ALT:  pL=$pL;  pR=$pR")
      val mm = p.m
      def long(q: Mark) = if (isInvalid(q)) -1 else mm - q.m
      val dL = long(pL)
      val dR = long(pR)
      // println(s"ALT: dL=$dL, dR=$dR")
      if (dL < 0 && dR < 0) invalid
      else if (dL > dR) (Left(vL),  pL)
      else if (dR > dL) (Right(vR), pR)
      else              (Left(vL),  pL)

    case SEQS(r1, r2, id) =>
      val Mark(n, m, _, _) = p
      val r1All =
        if (!nullable(r2)) invalid
        else back(r1, s, Mark(n, m), splits) match {
          case (v1, p1) if !isInvalid(p1) => (Sequ(v1, mkeps(r2)), p1)
          case _ => invalid
        }

      val kSplits = filterSplits(splits.getOrElse(id, Set.empty), p, nullable(r1), nullable(r2))

      val r1r2: (Val, Mark) =
        kSplits.map { k =>
            back(r1, s, Mark(n, k), splits) match {
              case (v1, p1) if !isInvalid(p1) =>
                // println(s"SEQS: k=$k, v1=$v1 p1=$p1")
                back(r2, s, Mark(k, m), splits) match {
                  case (v2, p2) if !isInvalid(p2) =>
                    // println(s"SEQS: k=$k, v1=$v1, v2=$v2")
                    (Sequ(v1, v2), p1)
                  case _ => invalid
                }
              case _ => invalid
            }
          }
          .collectFirst { case result if !isInvalid(result._2) => result }
          .getOrElse(invalid)

      val r2All =
        if (!nullable(r1)) invalid
        else back(r2, s, Mark(n, m), splits) match {
          case (v2, p2) if !isInvalid(p2) => (Sequ(mkeps(r1), v2), p2)
          case _ => invalid
        }

      (nullable(r1), nullable(r2)) match {
        case (true,  true)  => if (!isInvalid(r1All._2)) r1All else if (!isInvalid(r1r2._2)) r1r2 else r2All
        case (true,  false) => if (!isInvalid(r1r2._2)) r1r2 else r2All
        case (false, true)  => if (!isInvalid(r1All._2)) r1All else r1r2
        case (false, false) => r1r2
      }

    case STARSS(r, id) =>
      val Mark(n, m, _, _) = p

      def decodeStar(a: Int, b: Int): (Val, Mark) =
        if (a == b) (Stars(Nil), Mark(a, a))
        else {
          val kSplits =splits.getOrElse(id, Set.empty).filter(k => k > a && k <= b)
              .toList.sorted(Ordering.Int.reverse).iterator

          kSplits.map { k =>
              back(r, s, Mark(a, k), splits) match {
                case (v1, p1) if !isInvalid(p1) =>
                  decodeStar(k, b) match {
                    case (Stars(vs), pRest) if !isInvalid(pRest) =>
                      (Stars(v1 :: vs), p1)
                    case _ => invalid
                  }
                case _ => invalid
              }
            }
            .collectFirst { case res if !isInvalid(res._2) => res }
            .getOrElse(invalid)
        }

      decodeStar(n, m)

    case _ => invalid
  }

def back2(r: Rexp, s: String, p: Mark, splits: Map[Int, Set[Int]]): (Val, Mark) =
  r match {

    // ONE: same logic as before, ignore adv fields
    case ONE =>
      val Mark(n, m, _, _) = p
      if (n == m) (Empty, p) else invalid

    // CHAR: use the advancement slice (advStart, advEnd) to step back
    case CHAR(c) =>
      val Mark(n, m, advStart, advEnd) = p

      // We expect that the last CHAR step consumed exactly [advStart, advEnd) = [m-1, m)
      val okSlice  = (advEnd - advStart == 1) && advEnd == m && advStart >= n && advEnd <= s.length
      val okChar   = okSlice && s(advStart) == c

      if (okChar) {
        // Move back over that character: the previous global mark ends at advStart
        val prev = Mark(n, advStart) // adv info for the previous step is unknown, so default (-1,-1)
        (Chr(c), prev)
      } else {
        invalid
      }

    // ALT: recurse on both sides with the same mark p, as before, but using back2
    case ALT(r1, r2) =>
      val (vL, pL) = back2(r1, s, p, splits)
      val (vR, pR) = back2(r2, s, p, splits)

      val mm = p.m
      def long(q: Mark) = if (isInvalid(q)) -1 else mm - q.m

      val dL = long(pL)
      val dR = long(pR)

      if (dL < 0 && dR < 0) invalid
      else if (dL > dR) (Left(vL),  pL)
      else if (dR > dL) (Right(vR), pR)
      else              (Left(vL),  pL)

    // For now, delegate everything else to the old back.
    // We can later re-implement SEQS / STARSS / NTIMES here using advStart/advEnd.
    case _ =>
      back(r, s, p, splits)
  }

def filterSplits(
  initSplits: Set[Int],
  p: Mark,
  nullableR1: Boolean,
  nullableR2: Boolean
): Iterator[Int] = {
  val Mark(n, m, _, _) = p
  val inRange = initSplits.filter(k => k >= n && k <= m)
  val forR1   = if (nullableR1) inRange else inRange.filter(_ > n)
  val forR2   = if (nullableR2) forR1   else forR1.filter(_ < m)
  forR2.toList.sorted(Ordering.Int.reverse).iterator
}

@main
def test1() = {
  val reg = ("aa") | ("a" ~ (ONE ~ "a"))
  val s   = "aa"
  println(s"$s: r=\n${pp(reg)}  ${mat(reg, s)} ")
  println(lexer(reg, s))
  println(rebit.blexer(reg, s))
}

@main
def test2() = {
  val reg = ("a" ~ ("a" | ONE))
  val s   = "aa"
  println(s"$s: r=\n${pp(reg)} ${mat(reg, s)} ")
  println(lexer(reg, s))
  println(rebit.blexer(reg, s))
}

@main
def test3() = {
  val reg = %("a" | "aa")
  val s   = "aaaa"
  println(s"$s: r=\n${pp(reg)}  ")
  val vmark = lexer(reg, s)
  val vder  = rebit.blexer(reg, s)
  println(vmark)
  println(vder)
  println(vmark == vder)
}

@main
def testall() = {
  given rexp_cdata: CDATA[Rexp] = List(
    (0, _ => ONE),
    (0, _ => ZERO),
    (0, _ => CHAR('a')),
    (0, _ => CHAR('b')),
    (0, _ => CHAR('c')),
    (1, cs => mkSTARS(cs(0))),
    (2, cs => ALT(cs(0), cs(1))),
    (2, cs => mkSEQS(cs(0), cs(1)))
  )
  val alphabet = LazyList('a', 'b')

  for (i <- 0L to 1_000_000_000L) {
    val r = decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- generate_up_to(alphabet)(20)(r).take(19) if s != "") {
      val vm  = lexer(r, s)
      val vb  = rebit.blexer(r, s)
      val res = vm == vb
      if (!res) {
        println(s"$r and $s")
        println(s"vm=$vm vb=$vb")
        System.exit(1)
      }
    }
  }
}

// for measuring time
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (_ <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

def mkstar(n: Int) = STAR("a" * n)
def mkalts(n: Int) = {
  (for (i <- (1 to n).toList) yield mkstar(i)).reduceLeft(ALT.apply)
}
