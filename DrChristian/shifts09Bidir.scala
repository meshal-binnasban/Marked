//| mainClass: Main
//| scalaVersion: 3.8.3
//| scalacOptions: ["-deprecation", "-feature", "-language:implicitConversions", "-nowarn"]
//| mvnDeps:
//|   - org.scala-lang:scala3-library_3:3.8.3
//|   - org.scala-lang.modules::scala-parallel-collections:1.2.0
//| moduleDeps:
//|   - rexp.scala
//|   - regenerate.scala
//|   - re_bitrev3.scala
//|   - Generators.scala
//|   - enumpar.scala
//> using options -nowarn
//> using scala 3.8.3
//> using dep org.scala-lang.modules::scala-parallel-collections:1.2.0
//> using file rexp.scala
//> using file regenerate.scala
//> using file re_bitrev3.scala
//> using file Generators.scala
//> using file enumpar.scala
import scala.collection.parallel.CollectionConverters.*
import Rexp._
import Val._

// marks are now just sets of positions, not pairs
type Marks = Set[Int]

// each node carries two lazy arrays:
//   fwd(in)  = set of out positions reachable from in through this node
//   bwd(out) = set of in positions that can reach out through this node
// entries are null until first requested
enum RexpA:
  case ZEROA
  case ONEA
  case CHARA(c: Char)
  case ALTA(r1: RexpA, r2: RexpA,
            fwd: Array[Set[Int]],
            bwd: Array[Set[Int]])
  case SEQA(r1: RexpA, r2: RexpA,
            fwd: Array[Set[Int]],
            bwd: Array[Set[Int]])
  case STARA(r: RexpA,
             fwd: Array[Set[Int]],
             bwd: Array[Set[Int]])

import RexpA.*

def internalize(r: Rexp, n: Int): RexpA = r match {
  case ZERO    => ZEROA
  case ONE     => ONEA
  case CHAR(c) => CHARA(c)
  case ALT(r1, r2) =>
    ALTA(internalize(r1, n), internalize(r2, n),
         Array.ofDim(n + 1), Array.ofDim(n + 1))
  case SEQ(r1, r2) =>
    SEQA(internalize(r1, n), internalize(r2, n),
         Array.ofDim(n + 1), Array.ofDim(n + 1))
  case STAR(r) =>
    STARA(internalize(r, n),
          Array.ofDim(n + 1), Array.ofDim(n + 1))
}

// forward shifting: one structural pass, mark set is Set[Int]
// ms is the current set of positions; returns the set of reachable positions
def shiftFwd(r: RexpA, ms: Set[Int], s: String): Set[Int] = r match {
  case ZEROA => Set()
  case ONEA  => ms
  case CHARA(c) =>
    ms.collect { case m if m < s.length && s(m) == c => m + 1 }
  case ALTA(r1, r2, _, _) =>
    shiftFwd(r1, ms, s) ++ shiftFwd(r2, ms, s)
  case SEQA(r1, r2, _, _) =>
    shiftFwd(r2, shiftFwd(r1, ms, s), s)
  case st @ STARA(r, _, _) =>
    var seen     = ms
    var frontier = ms
    while
      val next = shiftFwd(r, frontier, s).diff(seen)
      frontier = next
      seen     = seen ++ next
      next.nonEmpty
    do ()
    seen
}

// backward shifting: mirror of forward
// ms is the current set of positions; returns the set of positions that led here
def shiftBwd(r: RexpA, ms: Set[Int], s: String): Set[Int] = r match {
  case ZEROA => Set()
  case ONEA  => ms
  case CHARA(c) =>
    ms.collect { case m if m > 0 && s(m - 1) == c => m - 1 }
  case ALTA(r1, r2, _, _) =>
    shiftBwd(r1, ms, s) ++ shiftBwd(r2, ms, s)
  case SEQA(r1, r2, _, _) =>
    shiftBwd(r1, shiftBwd(r2, ms, s), s)
  case st @ STARA(r, _, _) =>
    var seen     = ms
    var frontier = ms
    while
      val next = shiftBwd(r, frontier, s).diff(seen)
      frontier = next
      seen     = seen ++ next
      next.nonEmpty
    do ()
    seen
}

// lazy getFwd: compute fwd(node, in) once, cache in node.fwd(in)
def getFwd(r: RexpA, in: Int, s: String): Set[Int] = r match {
  case ZEROA   => Set()
  case ONEA    => Set(in)
  case CHARA(c) =>
    if (in < s.length && s(in) == c) Set(in + 1) else Set()
  case ALTA(_, _, fwd, _) =>
    if (fwd(in) != null) fwd(in)
    else { val res = shiftFwd(r, Set(in), s); fwd(in) = res; res }
  case SEQA(_, _, fwd, _) =>
    if (fwd(in) != null) fwd(in)
    else { val res = shiftFwd(r, Set(in), s); fwd(in) = res; res }
  case STARA(_, fwd, _) =>
    if (fwd(in) != null) fwd(in)
    else { val res = shiftFwd(r, Set(in), s); fwd(in) = res; res }
}

// lazy getBwd: compute bwd(node, out) once, cache in node.bwd(out)
def getBwd(r: RexpA, out: Int, s: String): Set[Int] = r match {
  case ZEROA   => Set()
  case ONEA    => Set(out)
  case CHARA(c) =>
    if (out > 0 && s(out - 1) == c) Set(out - 1) else Set()
  case ALTA(_, _, _, bwd) =>
    if (bwd(out) != null) bwd(out)
    else { val res = shiftBwd(r, Set(out), s); bwd(out) = res; res }
  case SEQA(_, _, _, bwd) =>
    if (bwd(out) != null) bwd(out)
    else { val res = shiftBwd(r, Set(out), s); bwd(out) = res; res }
  case STARA(_, _, bwd) =>
    if (bwd(out) != null) bwd(out)
    else { val res = shiftBwd(r, Set(out), s); bwd(out) = res; res }
}

// mkval: reconstruct the POSIX value for substring in..out
// no shifting is done here — only getFwd/getBwd calls (which are cached)
// and intersection to pick the POSIX split
def mkval(r: RexpA, in: Int, out: Int, s: String): Option[Val] = r match {
  case ZEROA => None

  case ONEA =>
    if (in == out) Some(Empty) else None

  case CHARA(c) =>
    if (s(in) == c) Some(Chr(c)) else None

  case ALTA(r1, r2, _, _) =>
    if (getFwd(r1, in, s).contains(out)) mkval(r1, in, out, s).map(Left(_))
    else mkval(r2, in, out, s).map(Right(_))

  case SEQA(r1, r2, _, _) =>
    // POSIX: largest mid = longest match by r1
    val mids = getFwd(r1, in, s) & getBwd(r2, out, s)
    if (mids.isEmpty) None
    else
      val mid = mids.max
      for v1 <- mkval(r1, in, mid, s); v2 <- mkval(r2, mid, out, s)
      yield Sequ(v1, v2)

  case st @ STARA(r, _, _) =>
    if (in == out) Some(Stars(Nil))
    else
      val valid = getFwd(st, in, s) & getBwd(st, out, s)
      var cur   = in
      var vs    = List.empty[Val]
      var fail  = false
      while cur != out && !fail do
        val next = (getFwd(r, cur, s) & valid).filter(_ > cur)
        if next.isEmpty then fail = true
        else
          val p = next.max
          mkval(r, cur, p, s) match
            case Some(v) => vs = vs :+ v; cur = p
            case None    => fail = true
      if fail then None else Some(Stars(vs))
}

def lexShift(r: Rexp, s: String): Option[Val] = {
  val n  = s.length
  val ar = internalize(r, n)
  // check if s.length is reachable from 0, then reconstruct
  if (getFwd(ar, 0, s).contains(n)) mkval(ar, 0, n, s)
  else None
}

// ── tests ────────────────────────────────────────────────────────────────────

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

def fromSeq[A](xs: Seq[A]): PushGen[A] = PushGen(v => xs.foreach(v))

def rexps(fuel: Int, alphabet: Seq[Char] = "abc".toSeq): PushGen[Rexp] =
  if fuel <= 0 then PushGen[Rexp](_ => ())
  else
    val sub  = rexps(fuel - 1, alphabet)
    val zero = PushGen[Rexp](v => v(ZERO))
    val one  = PushGen[Rexp](v => v(ONE))
    val chr  = fromSeq(alphabet).map(CHAR(_))
    val alt  = for r1 <- sub; r2 <- sub yield ALT(r1, r2)
    val seq  = for r1 <- sub; r2 <- sub yield SEQ(r1, r2)
    val star = sub.map(STAR(_))
    zero ++ one ++ chr ++ alt ++ seq ++ star

given rexp_enum: Enumeration[Rexp] = Enumeration.covering(List(
  (0, _  => ONE),
  (0, _  => ZERO),
  (0, _  => CHAR('a')),
  (0, _  => CHAR('b')),
  (0, _  => CHAR('c')),
  (1, cs => STAR(cs(0))),
  (2, cs => ALT(cs(0), cs(1))),
  (2, cs => SEQ(cs(0), cs(1)))
), maxIndex = 500_000_000L)

def testallS(maxRegexes: Long): Unit = {
  val alphabet = LazyList('a', 'b', 'c')
  var i = 0L
  rexps(6).foreach { r =>
    if (i >= maxRegexes) {
      println(s"No mismatches found in the first $i regexes.")
      throw new Exception("Reached maxRegexes")
    }
    i += 1
    if (i % 100_000 == 0) print("*")
    for (s <- generate_up_to(alphabet)(10)(r).take(10) if s != "") {
      val vm = lexShift(r, s) match {
        case Some(v) => v
        case None => throw new Exception(s"lexShift failed on $r and $s")
      }
      val vb = blexer(r, s)
      if (vm != vb) {
        println("Mismatch:")
        println(s"Regex: $r")
        println(s"String: $s")
        println(s"Bidir:  $vm")
        println(s"Blexer: $vb")
        sys.exit(1)
      }
    }
  }
  println(s"No mismatches found in the first $i regexes.")
}

def testallDS(maxIndex: Long): Unit = {
  val alphabet = LazyList('a', 'b', 'c')
  for (i <- 0L to maxIndex) {
    if (i % 100_000L == 0) print("*")
    val r = rexp_enum.decode(BigInt(i))
    for (s <- generate_up_to(alphabet)(10)(r).take(10) if s != "") {
      val vm = lexShift(r, s) match {
        case Some(v) => v
        case None => throw new Exception(s"lexShift failed on $r and $s")
      }
      val vb = blexer(r, s)
      if (vm != vb) {
        println()
        println("Mismatch:")
        println(s"Index:  $i")
        println(s"Regex:  $r")
        println(s"String: $s")
        println(s"Bidir:  $vm")
        println(s"Blexer: $vb")
        System.exit(1)
      }
    }
  }
  println()
  println(s"No mismatches found up to $maxIndex.")
}

@main
def measureS(maxRegexes: Long): Unit = {
  val t = time_needed(1, testallS(maxRegexes))
  println()
  println("========================================")
  println("Method: testallS")
  println("Enumeration: PushGen sequential")
  println(s"Number of regexes: $maxRegexes")
  println(s"Time: $t seconds")
  println("Result: no mismatch")
  println("========================================")
}

@main
def measureDS(maxRegexes: Long): Unit = {
  val t = time_needed(1, testallDS(maxRegexes))
  println()
  println("========================================")
  println("Method: testallDS")
  println("Enumeration: decode sequential")
  println(s"Number of regexes: $maxRegexes")
  println(s"Time: $t seconds")
  println("Result: no mismatch")
  println("========================================")
}

def tests(runs: Int = 100): Unit = {
  val examples = List[(Rexp, String)](
    ((ONE | "a") ~ ("aa" | "a"), "aa"),
    ((ONE | "a") ~ ("ab" | "b"), "ab"),
    ("aa" | ("a" ~ (ONE ~ "a")), "aa"),
    ((ONE ~ "a") | ("a" ~ ONE), "a"),
    ((("a" | "b") | "b"), "b"),
    ("a" | ("ab" | "ba"), "ab"),
    ((("a" | "ab") ~ ("b" | ONE)), "ab"),
    ("abc", "abc"),
    (((("aa") | "a") ~ ("a" | ("aa"))), "aaa"),
    ((("a" | ("aa")) ~ ("a" | ("aa"))), "aaa"),
    (((("a" | "c") ~ ("c" ~ "b")) | ((ZERO ~ ONE) ~ ONE)), "acb"),
    (ONE | "a", "a"),
    (((ZERO | "a") | ("a" | ZERO)), "a"),
    (((("b" ~ ONE) | %("b")) ~ %("b" | "c")), "bbc"),
    ((%(ONE) ~ "a"), "a"),
    (("a" | %("a")), "a"),
    ((ONE | %("a")), "a"),
    (%("a" | "aa"), "aaa"),
    ((%("a") | %("aa")), "aa"),
    (((ONE | "a") ~ %("a")), "a"),
    (((("a" ~ ONE) | (ONE ~ "a")) ~ %("a")), "aaaaaaaaa"),
    ((%("a" | "b")), "aba"),
    (("a" | ONE) ~ %("a"), ""),
    ((("a" | ONE) ~ "a") ~ %("a"), "aaa"),
    (("b" ~ ONE | %("a")) ~ %("a" | "b"), "aab"),
    (%("b" | %("a")), "bab")
  )

  def fmt(t: Double): String = f"${t}%.2e s"

  var inequality  = 0
  var bidirWins   = 0
  var blexerWins  = 0
  var totalBidir  = 0.0
  var totalBlexer = 0.0

  examples.zipWithIndex.foreach { case ((r, s), i) =>
    val bidir = lexShift(r, s) match {
      case Some(v) => v
      case _ => throw new Exception(s"lexShift failed on test ${i+1}")
    }
    val der     = blexer(r, s)
    val equal   = bidir == der
    if (!equal) inequality += 1

    val tBidir  = time_needed(runs, lexShift(r, s))
    val tBlexer = time_needed(runs, blexer(r, s))
    val diff    = tBidir - tBlexer
    totalBidir  += tBidir
    totalBlexer += tBlexer
    if (diff < 0) bidirWins += 1 else blexerWins += 1
    val winner  = if (diff < 0) "bidir" else "blexer"

    println(f"[${i+1}%2d] r=$r  s=$s")
    println(f"     equal=$equal  bidir=${fmt(tBidir)}  blexer=${fmt(tBlexer)}  diff=${fmt(diff)}  winner=$winner")
    println()
  }

  val n = examples.length
  println(s"Inequality : $inequality / $n")
  println(s"Bidir wins : $bidirWins / $n")
  println(s"Blexer wins: $blexerWins / $n")
  println(f"Avg bidir  : ${fmt(totalBidir / n)}")
  println(f"Avg blexer : ${fmt(totalBlexer / n)}")
}


  val reg = %( %("a") | %("aa") | %("aaa") | %("aaaa") | %("aaaaa") ) 
  val s   = "a" * 500
  