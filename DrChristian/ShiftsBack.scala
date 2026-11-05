//| mainClass: Main
//| scalaVersion: 3.8.3
//| scalacOptions: ["-deprecation", "-feature", "-language:implicitConversions" , "-nowarn"]
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

type Marks = Set[Int]

def shifts(ms: Marks, r: Rexp, s: String) : Marks = 
  r match {
    case ZERO => Set()
    case ONE => ms
    case CHAR(c) => for (m <- ms; if m < s.length && s(m) == c) yield m + 1
    case ALT(r1, r2) => shifts(ms, r1,s) ++ shifts(ms, r2,s)
    case SEQ(r1, r2) => shifts(shifts(ms, r1,s), r2,s)
    case STAR(r) => {
      val ms1 = shifts(ms, r,s).diff(ms)
      if (ms1 == Set()) ms else ms ++ shifts(ms1, STAR(r),s) 
    }
    //NOT case using shift back that returns pairs
    case NOT(r) =>
      val ms1 =shifts(ms, r, s)
      val mss =backShift(ms1.map(n => (n, n)), r, s)
        .filter { case (m, n) => ms.contains(m) }
      
      ms.flatMap { m =>
        val ms2 = for ((mm, n) <- mss if mm == m) yield n
        (m to s.length).toSet.diff(ms2)
        }

      // 1- get r reached positions
      // 2- backShift retrieve starts but paired with output (m:ms , n)
      //3- flat map for each m in ms: get all positions reached by r and take the difference
      //between that and all positions from m to s.length
      // similar to NOT case from before but no shifting.

}

//shiftsBack only returning starting points for r.
def shiftsBack(ms: Marks, r: Rexp, s: String) : Marks = r match {
    case ZERO => Set()
    case ONE => ms
    case CHAR(c) => for (m <- ms; if m > 0 && s(m-1) == c) yield m - 1
    case ALT(r1, r2) => shiftsBack(ms, r1,s) ++ shiftsBack(ms, r2,s)
    case SEQ(r1, r2) => shiftsBack(shiftsBack(ms, r2,s), r1,s)
    case STAR(r) => {
      val ms1 = shiftsBack(ms, r,s).diff(ms)
      if (ms1 == Set()) ms else ms ++ shiftsBack(ms1, STAR(r),s) 
    }
}

//backShift returning starting points for r paired with output positions plus choosing
//best output position.
def backShift(ms: Set[(Int, Int)], r: Rexp, s: String): Set[(Int, Int)] = r match {
  case ZERO => Set()
  case ONE => ms
  case CHAR(c) => for ((m, out) <- ms if m > 0 && s(m - 1) == c) yield (m - 1, out)

  
  //using groupBy starts, choosing max out
  case ALT(r1, r2) =>
    val ms1 = backShift(ms, r1, s)
    val ms2 = backShift(ms, r2, s)
    val grouped = (ms1 ++ ms2).groupBy(_._1).map {
      case (m, pairs) =>  (m, pairs.map(_._2).max)
    }
    grouped.toSet

  // using groupMapReduce attempt
  case ALT(r1, r2) =>
    val ms1 = backShift(ms, r1, s)
    val ms2 = backShift(ms, r2, s)
    println( (ms1 ++ ms2))
    println((ms1 ++ ms2).groupMapReduce(_._1)(_._2))
    (ms1 ++ ms2).groupMapReduce(_._1)(_._2)((a, b) => a.max(b)).toSet
  // with array attempt/
  case ALT(r1, r2) =>
    val ms1 = backShift(ms, r1, s)
    val ms2 = backShift(ms, r2, s)
    val best = Array.fill(s.length + 1)(-1)
    
    for ((m, out) <- ms1)
      if (out > best(m))
        best(m) = out

    for ((m, out) <- ms2)
      if (out > best(m))
        best(m) = out

    (for (m <- 0 to s.length if best(m) != -1)
      yield (m, best(m))).toSet

  case SEQ(r1, r2) => backShift(backShift(ms, r2, s), r1, s)

  // Updates may improve an already reached start, so propagate improvements.
  case STAR(r) =>
    val best = Array.fill(s.length + 1)(-1)
    for ((m, out) <- ms)
      if (out > best(m))
        best(m) = out
    var ms1 = ms
    while (ms1.nonEmpty) {
      val ms2 = backShift(ms1, r, s).filter { case (m, out) => out > best(m) }
      for ((m, out) <- ms2)
        best(m) = out
      ms1 = ms2
    }
    (for (m <- 0 to s.length if best(m) != -1) yield (m, best(m))).toSet

}

def mkval(r: Rexp, in: Int, out: Int, s: String): Val = r match {
  case ZERO => throw new Exception("mkval: ZERO cannot match") //?
  case ONE => Empty
  case CHAR(c) => Chr(c)

  case ALT(r1, r2) =>
    if (backShift(Set((out, out)), r1, s).exists(_._1 == in))
      Left(mkval(r1, in, out, s))
    else
      Right(mkval(r2, in, out, s))

  case SEQ(r1, r2) =>
    val ms1 = backShift(Set((out, out)), r2, s)
    val ms2 = backShift(ms1.map { case (m, _) => (m, m) }, r1, s)
    val mid = ms2.find(_._1 == in).get._2
    Sequ(mkval(r1, in, mid, s), mkval(r2, mid, out, s))

  case STAR(r) =>
    if (in == out) Stars(Nil)
    else {
      val ms1 = backShift(Set((out, out)), STAR(r), s)
      val ms2 = backShift(ms1.map { case (m, _) => (m, m) }, r, s)
      val mid = ms2.find(_._1 == in).get._2
      if (mid <= in) throw new Exception("mkval: STAR did not advance")
      mkval(STAR(r), mid, out, s) match {
        case Stars(vs) => Stars(mkval(r, in, mid, s) :: vs)
      }
    }
}

def matcher(r: Rexp, s: String): Boolean =
  shifts(Set(0), r, s).contains(s.length)

def lexShift(r: Rexp, s: String): Option[Val] =
  if (matcher(r, s)) Some(mkval(r, 0, s.length, s)) else None

//lexing with mkval directly.  
def lexMkval(r: Rexp, s: String): Option[Val] =
  try {
    Some(mkval2(r, 0, s.length, s))
  } catch {
    case _: Exception => None
  }



def backShift2(ms: Map[Int, Int], r: Rexp, s: String): Map[Int, Int] = r match {
  case ZERO => Map()
  case ONE => ms
  case CHAR(c) => (for ((m, out) <- ms if m > 0 && s(m - 1) == c) yield (m - 1, out)).toMap

  case ALT(r1, r2) =>
    val ms1 = backShift2(ms, r1, s)
    val ms2 = backShift2(ms, r2, s)
    ms1 ++ ms2.map {case (m, out) => (m, math.max(out, ms1.getOrElse(m, -1)))}

  case SEQ(r1, r2) => backShift2(backShift2(ms, r2, s), r1, s)

  case STAR(r) =>
    val ms1 = backShift2(ms, r, s).filter {
      case (m, out) => out > ms.getOrElse(m, -1)
    }

    if (ms1.isEmpty) ms
    else {
      ms ++ backShift2(ms1, STAR(r), s).map {
        case (m, out) => (m, math.max(out, ms.getOrElse(m, -1)))
      }
    }
 
}


def mkval2(r: Rexp, in: Int, out: Int, s: String): Val = r match {
 // case ZERO => throw new Exception("mkval2: ZERO cannot match")
  case ONE => Empty
  case CHAR(c) => Chr(c)

  case ALT(r1, r2) =>
    if (backShift2(Map(out -> out), r1, s).contains(in))
      Left(mkval2(r1, in, out, s))
    else
      Right(mkval2(r2, in, out, s))

  case SEQ(r1, r2) =>
    val ms1 = backShift2(Map(out -> out), r2, s)
    val ms2 = backShift2(ms1.keys.map(m => (m, m)).toMap, r1, s)
    val mid = ms2(in)

    Sequ(mkval2(r1, in, mid, s), mkval2(r2, mid, out, s))

  case STAR(r) =>
    if (in == out) Stars(Nil)
    else {
      val ms1 = backShift2(Map(out -> out), STAR(r), s)
      val ms2 = backShift2(ms1.keys.map(m => (m, m)).toMap, r, s)
      val mid = ms2(in)

      mkval2(STAR(r), mid, out, s) match {
        case Stars(vs) => Stars(mkval2(r, in, mid, s) :: vs)
      }
    }
}

def lexShift2(r: Rexp, s: String): Option[Val] =
  if (matcher(r, s))
    Some(mkval2(r, 0, s.length, s))
  else
    None






//================================Tests========================================

def fromSeq[A](xs: Seq[A]): PushGen[A] = PushGen(v => xs.foreach(v)) 
  // enumerating Rexps using by default "ab" as alphabet
  def rexps(fuel: Int, alphabet: Seq[Char] = "abc".toSeq): PushGen[Rexp] =
    if fuel <= 0 then PushGen[Rexp](_ => ())            // nothing fits in depth 0
    else
      val sub  = rexps(fuel - 1, alphabet)              // built once, reused below
      val zero = PushGen[Rexp](v => v(ZERO))       // nullary leaves
      val one  = PushGen[Rexp](v => v(ONE))
      val chr  = fromSeq(alphabet).map(CHAR(_))    // leaf carrying a Char
      val alt  = for r1 <- sub; r2 <- sub yield ALT(r1, r2)   // binary
      val seq  = for r1 <- sub; r2 <- sub yield SEQ(r1, r2)   // binary
      val star = sub.map(STAR(_))                  // unary
      zero ++ one ++ chr ++ alt ++ seq ++ star

given rexp_enum: Enumeration[Rexp] = Enumeration.covering(List(
  (0, _  => ONE),
  (0, _  => ZERO),
  (0, _  => CHAR('a')),
  (0, _  => CHAR('b')),
  (0, _  => CHAR('c')),
  (1, cs => STAR(cs(0))),
  (1, cs => NOT(cs(0))),
  (2, cs => ALT(cs(0), cs(1))),
  (2, cs => SEQ(cs(0), cs(1)))
), maxIndex = 500_000_000L)


//testing function using pushGen
def allLexer(maxRegexes: Long): Unit = {
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
      val vm = lexMkval(r, s) match {
        case Some(v) => v
        case None => throw new Exception(s"lexMkval failed on $r and $s")
      }
      val vb = blexer(r, s)
      if (vm != vb) {
        println("Mismatch:")
        println(s"Regex: $r")
        println(s"String: $s")
        println(s"Marks: $vm")
        println(s"Derivative: $vb")
        sys.exit(1)
      }
    }
  }

  println(s"No mismatches found in the first $i regexes.")
}

//main funciton for testing using pushGen with time report
@main def measureAllLexer(maxRegexes: Long): Unit = {
  println("========================================")
  println("Method: testallS")
  println("Enumeration: PushGen sequential")
  println(s"Number of regexes: $maxRegexes")
  var t=0.0
  try {
    t = time_needed(1, allLexer(maxRegexes))
    println(s"Time: $t seconds")
  } catch {
    case e: Exception =>
      println()
      println(e.getMessage)
      println(s"Time: $t seconds")
  }

  println("========================================")
}

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
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
    (%("b" | %("a")), "bab"),
    (%("a" | "aa"), ("a" * 1))
  )

  def fmt(t: Double): String = f"${t}%.2e s"

  var inequality  = 0
  var bidirWins   = 0
  var blexerWins  = 0
  var totalBidir  = 0.0
  var totalBlexer = 0.0

  examples.zipWithIndex.foreach { case ((r, s), i) =>
    val bidir = lexMkval(r, s) match {
      case Some(v) => v
      case _ => throw new Exception(s"shifts and backshifts lexMkval failed on test ${i+1}")
    }
    val der     = blexer_simp(r, s)
    val equal   = bidir == der
    if (!equal) inequality += 1

    val tBidir  = time_needed(runs, lexMkval(r, s))
    val tBlexer = time_needed(runs, blexer_simp(r, s))
    val diff    = tBidir - tBlexer
    totalBidir  += tBidir
    totalBlexer += tBlexer
    if (diff < 0) bidirWins += 1 else blexerWins += 1
    val winner  = if (diff < 0) "bidir" else "blexer"

    println(f"[${i+1}%2d] r=$r  s=$s")
    println(f"     equal=$equal  backshifts=${fmt(tBidir)}  blexer=${fmt(tBlexer)}  diff=${fmt(diff)}  winner=$winner")
    println()
  }

  val n = examples.length
  println(s"Inequality : $inequality / $n")
  println(s"Backshifts wins : $bidirWins / $n")
  println(s"derivatives wins: $blexerWins / $n")
  println(f"Avg backshifts  : ${fmt(totalBidir / n)}")
  println(f"Avg derivatives : ${fmt(totalBlexer / n)}")
}
