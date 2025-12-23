file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/shifts08.sc
### scala.MatchError: TypeDef(Marks,AppliedTypeTree(Ident(Set),List(Ident(Mark)))) (of class dotty.tools.dotc.ast.Trees$TypeDef)

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 3211
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/shifts08.sc
text:
```scala
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
def shifts(ms: Marks, s: String, r: Rexp): Marks = r match {
  case ZERO => Set()
  case ONE  => Set()

  case CHARS(c, id) => (for {Mark(n, m) <- ms if m < s.length && s(m) == c} yield Mark(n, m + 1))

  case ALTS(r1, r2,id) => shifts(ms, s, r1) ++ shifts(ms, s, r2)
  case SEQS(r1, r2, id) =>
    val ms1  = shifts(ms, s, r1)

    //this checks to add the possible split of skipping r1.
    val inR2 = if (nullable(r1)) ms1 ++ ms else ms1

    //this records the splits of r1. for now i am using a global variable
    //just to check if it can work.
    inR2.foreach(q => recordSplit(id, q.m))
    (nullable(r1), nullable(r2)) match {
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

  case NTIMES(r, n) =>
    if (n == 0) Set()
    else if (n == 1) shifts(ms, s, r)
    else {
      val ms1 = shifts(ms, s, r)
      if (ms1 == Set()) Set()
      else if (nullable(r)) ms1 ++ shifts(ms1, s, NTIMES(r, n - 1))
      else shifts(ms1, s, NTIMES(r, n - 1))
    }
}

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
    if (nullable(r)) mkeps(r) else Invalid
  } else {
    val ms     = mat(r, s)
    val finals = ms.filter(_.m == s.length)
    if (finals.isEmpty) Invalid
    else {
      val best: Mark = finals.head
      val (v, p0) = back(r, s, best, splits = splits)
      if (isInvalid(p0)) Invalid else v
    }
  }
}

//helper function to check if a mark is invalid, for use in back function.
def isInvalid(q: Mark): Boolean = q.n < 0

//this defines an invalid return, with value and invalid mark.@@
val invalid = (Invalid, Mark(-1, -1))

def back(r: Rexp, s: String, p: Mark, splits: Map[Int, Set[Int]]): (Val, Mark) =
  // println(s"splits=$splits")
  r match {

    case ONE =>
      val Mark(n, m) = p
      if (n == m) (Empty, p) else invalid

    case CHARS(c, id) =>
      val Mark(n, m) = p
      if (m == n + 1 && m <= s.length && s(n) == c) (Chr(c), Mark(n, m - 1))
      else invalid

    case ALTS(r1, r2, id) =>
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
      val Mark(n, m) = p
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
      val Mark(n, m) = p

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

def filterSplits(
  initSplits: Set[Int],
  p: Mark,
  nullableR1: Boolean,
  nullableR2: Boolean
): Iterator[Int] = {
  val Mark(n, m) = p
  val inRange = initSplits.filter(k => k >= n && k <= m)
  val forR1   = if (nullableR1) inRange else inRange.filter(_ > n)
  val forR2   = if (nullableR2) forR1   else forR1.filter(_ < m)
  forR2.toList.sorted(Ordering.Int.reverse).iterator
}

@main
def testExamples(): Unit = {

  val tests: List[(String, Rexp, String)] = List(
    ("test1",  %("a" | "aa"), "aa"),
    ("test2",  %("a"), "aaa"),
    ("test3",  %("aa") ~ %(%("a")), "aaaaaa"),
    ("test4",  (%("a") ~ %("a")) ~ "a", "aaa"),
    ("test5",  %(%(%("a"))), "aa"),
    ("test6",  %("a" ~ %("a")), "aaa"),
    ("test7",  %(%("a") ~ "a"), "aaa"),
    ("test8",  %("a") ~ (%("a") ~ "a"), "aaa"),
    ("test9",  %("a") ~ %("a"), "a" * 10),
    ("test10", %((ONE | "a") | "aa"), "aaa"),
    ("test11", (ONE | "a") ~ ("a" | "aa"), "aaa"),
    ("test12", ("a" | "ab") ~ ("c" | "bc"), "abc"),
    ("test13", (("a" | "b") | "ab") ~ ("bc" | "c" | "b"), "abc"),
    ("test14", (ONE | (ONE | "bc")) | (("a" | ONE) ~ ("a" | "aa")), "aa"),
    ("test15", (("a" | "b") ~ (ONE | "a")) ~ "a", "aa"),
    ("test16", ((ONE | "c") | %(ONE)) ~ "b", "b"),
    ("test17", %("a" ~ ONE), "aa"),
    ("test18", %("a" | %("b")), "ba"),
    ("test19", %(%("a") | "ab"), "aabaab"),
    ("test20", %("bb" | "b"), "bbb"),
    ("test21", ("a" ~ %("b")) ~ (("b" | ONE) ~ "a"), "aba"),
    ("test22", ("c" | ("a" | ONE)) ~ ("ab" | "b"), "ab"),
    ("test23", NTIMES("a", 3), "a" * 3),
    ("test24", %("a") ~ %("a"), "a" * 4)
  )

  def printSplitsTable(splits: Map[Int, Set[Int]]): Unit = {
    if (splits.isEmpty) {
      println("Splits: (empty)")
      return
    }

    val rows = splits.toList.map { case (id, ks) => (id.toString, ks.toString) }

    val idW  = math.max("id".length, rows.map(_._1.length).max)
    val ksW  = math.max("ks".length, rows.map(_._2.length).max)

    def line(ch: Char): String =
      "+" + (ch.toString * (idW + 2)) + "+" + (ch.toString * (ksW + 2)) + "+"

    def row(a: String, b: String): String =
      "| " + a.padTo(idW, ' ') + " | " + b.padTo(ksW, ' ') + " |"

    println(line('-'))
    println(row("id", "ks"))
    println(line('='))
    rows.sortBy(_._1.toInt).foreach { case (id, ks) => println(row(id, ks)) }
    println(line('-'))
  }

  var passed = 0
  var failed = 0

  tests.zipWithIndex.foreach { case ((label, r, s), i) =>

    val vDer = rebit.blexer(r, s)

    val (vMark, finals): (Val, Set[Mark]) =
      if (s == "") {
        val vm: Val = if (nullable(r)) mkeps(r) else Invalid
        (vm, Set.empty[Mark])
      } else {
        val ms: Set[Mark] = mat(r, s)
        val finals0: Set[Mark] = ms.filter(_.m == s.length)
        if (finals0.isEmpty) (Invalid: Val, Set.empty[Mark])
        else {
          val best = finals0.head
          val (v, p0) = back(r, s, best, splits)
          val vm: Val = if (isInvalid(p0)) Invalid else v
          (vm, finals0)
        }
      }

    val ok = (vMark == vDer)
    if (ok) passed += 1 else failed += 1

    println("==================================================")
    println(s"Test ${i + 1}: $label   [${if (ok) "OK" else "FAIL"}]")
    println("Regex:")
    println(pp(r))
    println(s"Input: $s")
    if (s != "") println(s"Final marks: ${finals.toList.sortBy(m => (m.n, m.m)).mkString(", ")}")
    println(s"Marked value    : $vMark")
    println(s"Derivative value: $vDer")
    printSplitsTable(splits)
  }

  println("\n==== Summary ====")
  println(s"Passed: $passed")
  println(s"Failed: $failed")
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

```



#### Error stacktrace:

```
dotty.tools.pc.completions.KeywordsCompletions$.checkTemplateForNewParents$$anonfun$2(KeywordsCompletions.scala:218)
	scala.Option.map(Option.scala:242)
	dotty.tools.pc.completions.KeywordsCompletions$.checkTemplateForNewParents(KeywordsCompletions.scala:215)
	dotty.tools.pc.completions.KeywordsCompletions$.contribute(KeywordsCompletions.scala:44)
	dotty.tools.pc.completions.Completions.completions(Completions.scala:126)
	dotty.tools.pc.completions.CompletionProvider.completions(CompletionProvider.scala:139)
	dotty.tools.pc.ScalaPresentationCompiler.complete$$anonfun$1(ScalaPresentationCompiler.scala:150)
```
#### Short summary: 

scala.MatchError: TypeDef(Marks,AppliedTypeTree(Ident(Set),List(Ident(Mark)))) (of class dotty.tools.dotc.ast.Trees$TypeDef)