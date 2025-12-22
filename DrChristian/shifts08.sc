
import scala.language.implicitConversions

import $file.rexps, rexps._
import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit

case class Mark(
  n: Int,
  m: Int,
)

type Marks = Set[Mark]

//this map stores all splits for each seq as shifting
//is done and also for stars using ids that i have added to the constructors
var splits: Map[Int, Set[Int]] = Map.empty

// this is the function to update splits.
def recordSplit(id: Int, k: Int): Unit =
  splits = splits.updated(id, splits.getOrElse(id, Set.empty) + k)

// shifts function 
def shifts(ms: Marks, s: String, r: RexpS): Marks = r match {
  case ZEROS => Set()
  case ONES  => Set()

  case CHARS(c) => (for { Mark(n, m) <- ms if m < s.length && s(m) == c } yield Mark(n, m + 1))

  case ALTS(r1, r2) => shifts(ms, s, r1) ++ shifts(ms, s, r2)

  case SEQS(r1, r2, id) =>
    val ms1  = shifts(ms, s, r1)

/*     
   //this check is to add the possible splits of skipping r1, the original ms.
    val inR2 = if (nullableS(r1)) ms1 ++ ms else ms1
    inR2.foreach(q => recordSplit(id, q.m))
    //but now i think it is not needed, but it is what i tried first and tested.
 */

    //this records the splits of r1. for now i am using a global splits map
    //just to check if it can work.
    ms1.foreach(q => recordSplit(id, q.m))

    (nullableS(r1), nullableS(r2)) match {
      case (true,  true)  => shifts(ms1 ++ ms, s, r2) ++ ms1
      case (true,  false) => shifts(ms1 ++ ms, s, r2)
      case (false, true)  => ms1 ++ shifts(ms1, s, r2)
      case (false, false) => shifts(ms1, s, r2)
    }

  case STARSS(r, id) =>
    val ms1 = shifts(ms, s, r)

    //this is similar to seq, but the splits is how much advancment was
    //made after one iteration of r.
    ms1.foreach(q => recordSplit(id, q.m))

    if (ms1.isEmpty) Set()
    else ms1 ++ shifts(ms1, s, STARSS(r, id))

  case NTIMESS(r, n) =>
    if (n == 0) Set()
    else if (n == 1) shifts(ms, s, r)
    else {
      val ms1 = shifts(ms, s, r)
      if (ms1 == Set()) Set()
      else if (nullableS(r)) ms1 ++ shifts(ms1, s, NTIMESS(r, n - 1))
      else shifts(ms1, s, NTIMESS(r, n - 1))
    }
  case ANDS(r1, r2) =>
    shifts(ms, s, r1) intersect shifts(ms, s, r2)
}

def mat(r: Rexp, s: String): Marks = {
  splits = Map.empty
  shifts(Set(Mark(0, 0)), s, intern(r))
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
    if (nullable(r)) mkeps(r) else Invalid
  } else {
    val ms     = mat(r, s)
    val finals = ms.filter(_.m == s.length)
    if (finals.isEmpty) Invalid
    else {
      val best: Mark = finals.head
     // println(ppp(rexps.intern(r)))
     // println(s"splits=$splits")
      val (v, p0) = back(r, s, best, splits = splits)
      if (isInvalid(p0)) Invalid else v
    }
  }
}

//helper function to check if a mark is invalid, for use in back function.
def isInvalid(q: Mark): Boolean = q.n < 0

//this defines an invalid return, with value and invalid mark.
val invalid = (Invalid, Mark(-1, -1))

def back(r: Rexp, s: String, p: Mark, splits: Map[Int, Set[Int]]): (Val, Mark) =
  r match {
    case ONE =>
      val Mark(n, m) = p
      if (n == m) (Empty, p) else invalid

    case CHAR(c) =>
      val Mark(n, m) = p
      if (m == n + 1 && m <= s.length && s(n) == c)
        (Chr(c), Mark(n, m - 1))
      else invalid

    case ALT(r1, r2) =>
      val (vL, pL) = back(r1, s, p, splits)
      val (vR, pR) = back(r2, s, p, splits)
      val mm = p.m
      def long(c: Mark) = if (isInvalid(c)) -1 else mm - c.m
      val dL = long(pL)
      val dR = long(pR)

      //call back on both sides, and compare advancement using long helper.
      if (dL < 0 && dR < 0) invalid
      else if (dL > dR) (Left(vL),  pL)
      else if (dR > dL) (Right(vR), pR)
      else              (Left(vL),  pL)

    case SEQ(r1, r2) =>
      val Mark(n, m) = p

      // here the r1 consumes all, so back is called with n,m leaving nothing for r2.
      val r1All =
        if (!nullable(r2)) invalid
        else back(r1, s, Mark(n, m), splits) match {
          case (v1, p1) if !isInvalid(p1) => (Sequ(v1, mkeps(r2)), p1)
          case _ => invalid
        }

      //in here, i tried to filter the splits before trying them,
      //the helper filters for example cases if r1 is not nullable,
      //so it the split k must be greater than n indicating some consumption
      val kSplits =
        filterSplits(
          splits.values.flatten.toSet,
          p,
          nullable(r1),
          nullable(r2)
        )

      val r1r2: (Val, Mark) =
        kSplits.map { k =>
          back(r1, s, Mark(n, k), splits) match {
            case (v1, p1) if !isInvalid(p1) =>
              back(r2, s, Mark(k, m), splits) match {
                case (v2, p2) if !isInvalid(p2) =>
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

    case STAR(r) =>
      val Mark(n, m) = p

      //here, back tries every split or advancment recorder by the star starting with larger splits.
      def decodeStar(a: Int, b: Int): (Val, Mark) =
        if (a == b) (Stars(Nil), Mark(a, a))
        else {
          val kSplits =
            splits.values.flatten
              .filter(k => k > a && k <= b)
              .toList.sorted(Ordering.Int.reverse).iterator

          kSplits.map { k =>
            //start the largest split k and if call from k,b stopping when they are equal
            //the problem is that it must try all splits to determine the splits that consumes all.
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

//helper to filter splits
def filterSplits(initSplits: Set[Int], p: Mark, nullableR1: Boolean, nullableR2: Boolean): Iterator[Int] = {
  val Mark(n, m) = p
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
def testall() = {
  given rexp_cdata: CDATA[Rexp] = List(
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
