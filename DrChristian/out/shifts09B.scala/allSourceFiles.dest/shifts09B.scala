//SOURCECODE_ORIGINAL_FILE_PATH=/Users/meshalbinnasban/Google Drive/KCL/Code Playground/Marked/DrChristian/shifts09B.scala
//SOURCECODE_ORIGINAL_CODE_START_MARKER
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

def shifts(ms: Marks, r: Rexp, s: String) : Marks = r match {
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

//shift back returns pairs (start, out) instead of only starts  
def backShift(ms: Set[(Int, Int)], r: Rexp, s: String): Set[(Int, Int)] = r match {
  case ZERO =>Set()
  case ONE => ms
  case CHAR(c) => for ((m, out) <- ms; if m > 0 && s(m - 1) == c) yield (m - 1, out)
  case ALT(r1, r2) => backShift(ms, r1, s) ++ backShift(ms, r2, s)
  case SEQ(r1, r2) => backShift(backShift(ms, r2, s), r1, s)
  case STAR(r) =>
    val ms1 = backShift(ms, r, s).diff(ms)
    if (ms1 == Set()) ms else ms ++ backShift(ms1, STAR(r), s)
  case NOT(r) =>
    val all = for { (m, out) <- ms;  m1 <- 0 to m } yield (m1, out)
    val mss = backShift(ms, r, s)
    all.diff(mss)
}

//mkval with in,out as integers
def mkval(r: Rexp, in: Int, out: Int, s: String): Val = r match {
  case ONE => Empty
  case CHAR(c) => Chr(c)
  case ALT(r1, r2) =>
    if (shifts(Set(in), r1, s).contains(out)) 
      Left(mkval(r1, in, out, s))
    else
      Right(mkval(r2, in, out, s))
  case SEQ(r1, r2) =>
    val mid =shifts(Set(in), r1, s).intersect(shiftsBack(Set(out), r2, s)).max
    Sequ(mkval(r1, in, mid, s), mkval(r2, mid, out, s))
  case STAR(r) =>
    if (in == out) Stars(Nil)
    else {
      val mid = shifts(Set(in), r, s).intersect(shiftsBack(Set(out), STAR(r), s))
          .max //.filter(_ > in)
      mkval(STAR(r), mid, out, s) match {
        case Stars(vs) => Stars(mkval(r, in, mid, s) :: vs)
      }
    }
}

def matcher(r: Rexp, s: String): Boolean =
  shifts(Set(0), r, s).contains(s.length)

def lexShift(r: Rexp, s: String): Option[Val] =
  if (matcher(r, s)) Some(mkval(r, 0, s.length, s)) else None

def lexMkval(r: Rexp, s: String): Option[Val] =
  try {
    Some(mkval(r, 0, s.length, s))
  } catch {
    case _: Exception => None
  }

//================================String Shifts ========================================
type MarksS = Set[String]

def shiftsS(ms: MarksS, r: Rexp): MarksS = r match {
  case ZERO => Set()
  case ONE => ms
  case CHAR(c) => for (m <- ms; if m != "" && m.head == c) yield m.tail
  case ALT(r1, r2) => shiftsS(ms, r1) ++ shiftsS(ms, r2)
  case SEQ(r1, r2) => shiftsS(shiftsS(ms, r1), r2)
  case STAR(r) =>
    val ms1 = shiftsS(ms, r).diff(ms)
    if (ms1 == Set()) ms else ms ++ shiftsS(ms1, STAR(r))
}

def shiftsBackS(ms: MarksS, r: Rexp, s: String): MarksS = r match {
  case ZERO => Set()
  case ONE => ms
  case CHAR(c) => for (m <- ms; if s.endsWith(c.toString + m)) yield c.toString + m
  case ALT(r1, r2) => shiftsBackS(ms, r1, s) ++ shiftsBackS(ms, r2, s)
  case SEQ(r1, r2) => shiftsBackS(shiftsBackS(ms, r2, s), r1, s)
  case STAR(r) =>
    val ms1 = shiftsBackS(ms, r, s).diff(ms)
    if (ms1 == Set()) ms else ms ++ shiftsBackS(ms1, STAR(r), s)
}

//mkval wih in,out as strings
def mkvalS(r: Rexp, in: String, out: String, s: String): Val = r match {
  case ONE =>Empty
  case CHAR(c) =>Chr(c)
  case ALT(r1, r2) =>
    if (shiftsS(Set(in), r1).contains(out))
      Left(mkvalS(r1, in, out, s))
    else
      Right(mkvalS(r2, in, out, s))
  case SEQ(r1, r2) =>
    val mid =shiftsS(Set(in), r1).intersect(shiftsBackS(Set(out), r2, s))
    .minBy(_.length)
    Sequ(mkvalS(r1, in, mid, s), mkvalS(r2, mid, out, s))
  case STAR(r) =>
    if (in == out) Stars(Nil)
    else {
      val mid =shiftsS(Set(in), r).intersect(shiftsBackS(Set(out), STAR(r), s))
          .filter(_.length < in.length).minBy(_.length)

      mkvalS(STAR(r), mid, out, s) match {
        case Stars(vs) => Stars(mkvalS(r, in, mid, s) :: vs)
      }
    }
}
def lexShiftS(r: Rexp, s: String): Option[Val] =
  if (shiftsS(Set(s), r).contains("")) Some(mkvalS(r, s, "", s)) else None


//================================Shift Back with no out ========================================
//shifts back with no out mark
def shiftsBackPure(ms: MarksS, r: Rexp, max: Int): MarksS = r match {
  case ZERO => Set()
  case ONE => ms
  case CHAR(c) => for (m <- ms; if m.length < max) yield c.toString + m
  case ALT(r1, r2) =>shiftsBackPure(ms, r1, max) ++ shiftsBackPure(ms, r2, max)
  case SEQ(r1, r2) =>shiftsBackPure(shiftsBackPure(ms, r2, max), r1, max)
  case STAR(r) =>
    val ms1 = shiftsBackPure(ms, r, max).diff(ms)
    if (ms1 == Set()) ms else ms ++ shiftsBackPure(ms1, STAR(r), max)
}
def matcherBackPure(r: Rexp, s: String): Boolean =
  shiftsBackPure(Set(""), r, s.length).contains(s)

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
      val vm = lexShift(r, s) match {
        case Some(v) => v
        case None => throw new Exception(s"lexShift failed on $r and $s")
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


def tests(): Unit = {
  val examples = List[(Rexp, String)](
    ((ONE | "a") ~ ("aa" | "a"), "aa"), //1
    ((ONE | "a") ~ ("ab" | "b"), "ab"),//2
    ("aa" | ("a" ~ (ONE ~ "a")), "aa"), //3
    ((ONE ~ "a") | ("a" ~ ONE), "a"),//4
    ((("a" | "b") | "b"), "b"),//5
    ("a" | ("ab" | "ba"), "ab"), //6
    ((("a" | "ab") ~ ("b" | ONE)), "ab"), //7
    ("abc", "abc"), //8
    (((("aa") | "a") ~ ("a" | ("aa"))), "aaa"),//9
    ((("a" | ("aa")) ~ ("a" | ("aa"))), "aaa"), //10
    (((("a" | "c") ~ ("c" ~ "b")) | ((ZERO ~ ONE) ~ ONE)), "acb"),//11
    (ONE | "a", "a"),//12
    (( ( ZERO |"a" ) | ( "a"|ZERO )) , "a"),//13

    (((("b" ~ ONE) | %("b")) ~ %("b" | "c")), "bbc"),//14
    ((%(ONE) ~ "a"), "a"),//15
    (("a" | %("a")), "a"),//16
    ((ONE | %("a")), "a"),//17
    (%("a" | "aa"), "aaa"),//18
    ((%("a") | %("aa")), "aa"),//19
    (((ONE | "a") ~ %("a")), "a"),//20
    (((("a" ~ ONE) | (ONE ~ "a")) ~ %("a")), "aaaaaaaaa"), //21
    ((%("a" | "b")), "aba"), //22
    (("a" | ONE) ~ %("a"), ""), //23
    ((("a" | ONE) ~ "a") ~ %("a"), "aaa"), //24
    (("b" ~ ONE | %("a")) ~ %("a" | "b"), "aab"),//25
    (%("b" | %("a")), "bab")//26
  )

  var inequality = 0

  examples.zipWithIndex.foreach { case ((r, s), i) =>
    val marks = lexShift(r, s) match {
      case Some(v) => v
      case _ => throw new Exception("mkval no value")
    }
    val der = blexer(r, s)

    if (marks != der) inequality += 1

    println(s"Test ${i + 1}")
    println(s"Regex: $r")
    println(s"Input: $s")
    println(s"Marks: ${marks}")
    println(s"Derivative: ${der}")
    println(s"Equal: ${marks == der}")
    println()
  }

  println(s"Inequality: $inequality / ${examples.length}")
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
    val bidir = lexShift(r, s) match {
      case Some(v) => v
      case _ => throw new Exception(s"shifts and backshifts lexShift failed on test ${i+1}")
    }
    val der     = blexer_simp(r, s)
    val equal   = bidir == der
    if (!equal) inequality += 1

    val tBidir  = time_needed(runs, lexShift(r, s))
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

def testsNot(): Unit =
  List[(Rexp, String, Boolean)](
    (NOT("a"), "", true),
    (NOT("a"), "a", false),
    (NOT("a"), "b", true),
    (NOT("a"), "aa", true),

    (NOT("a" | "aa"), "a", false),
    (NOT("a" | "aa"), "aa", false),
    (NOT("a" | "aa"), "aaa", true),
    (NOT("a" | "aa"), "b", true),

    ((ONE | "a") ~ NOT("a"), "a", true),
    ((ONE | "a") ~ NOT("a"), "aa", true),
    ((ONE | "a") ~ NOT("a"), "aaa", true),

    (SEQ(NOT("aa"), NOT("a")), "a", true),
    (SEQ(NOT("aa"), NOT("a")), "aa", true),
    (SEQ(NOT("aa"), NOT("a")), "aaa", true),

    (NOT(%("a")), "", false),
    (NOT(%("a")), "a", false),
    (NOT(%("a")), "aaa", false),
    (NOT(%("a")), "aaab", true),

    (NOT((%("a" | "b") ~ "aa") ~ %("a" | "b")), "", true),
    (NOT((%("a" | "b") ~ "aa") ~ %("a" | "b")), "a", true),
    (NOT((%("a" | "b") ~ "aa") ~ %("a" | "b")), "b", true),
    (NOT((%("a" | "b") ~ "aa") ~ %("a" | "b")), "ab", true),
    (NOT((%("a" | "b") ~ "aa") ~ %("a" | "b")), "aba", true),
    (NOT((%("a" | "b") ~ "aa") ~ %("a" | "b")), "abba", true),
    (NOT((%("a" | "b") ~ "aa") ~ %("a" | "b")), "aa", false),
    (NOT((%("a" | "b") ~ "aa") ~ %("a" | "b")), "aab", false),
    (NOT((%("a" | "b") ~ "aa") ~ %("a" | "b")), "baa", false),
    (NOT((%("a" | "b") ~ "aa") ~ %("a" | "b")), "baab", false)
  ).zipWithIndex.foreach { case ((r, s, expected), i) =>
    
    println(s"\n--- Test ${i + 1} ---")
    val marks = matcher(r,s)
    val der = derMatcher(r, s)

    
    println(s"Regex: $r")
    println(s"Input: $s")
    println(s"Marks: $marks ,  Derivative: $der")
    println(s"Expected: $expected\n")
    println(s"Equal to Derivative: ${marks == der}")

  
    println(s"--- End of Test ${i + 1} ---\n")
  }

  //testing function using pushGen


def allMatcher(maxRegexes: Long): Unit = {
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
      val vm = matcher(r, s) 
      val vb = derMatcher(r, s)
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
@main def measureAllMatcher(maxRegexes: Long): Unit = {
  println("========================================")
  println("Method: allMatcher")
  println("Enumeration: PushGen sequential")
  println(s"Number of regexes: $maxRegexes")
  var t=0.0
  try {
    t=time_needed(1, allMatcher(maxRegexes))
    println(s"Time: $t seconds")
  } catch {
    case e: Exception =>
      println()
      println(e.getMessage)
      println(s"Time: $t seconds")
  }

  println("========================================")
}


/*     

shifts:
case STAR(r) =>
      var seen     = ms
      var frontier = ms
      while
        val next = shiftsBack(frontier, r, s).diff(seen)
        frontier = next
        seen     = seen ++ next
        next.nonEmpty
      do ()
      seen 
      
      
shifts back
    case STAR(r) =>
      var seen     = ms
      var frontier = ms
      while
        val next = shifts(frontier, r, s).diff(seen)
        frontier = next
        seen     = seen ++ next
        next.nonEmpty
      do ()
      seen

mkval

//simple while loop and shifting r inside.
  case STAR(r) =>
    val valid = shiftsBack(Set(out), STAR(r), s) //shifts(Set(in), STAR(r), s) & 
    var cur = in
    var vs = List.empty[Val]
    while cur != out do
      val p = (shifts(Set(cur), r, s) & valid).filter(_ > cur).max
      vs = mkval(r, cur, p, s) :: vs
      cur = p
    Stars(vs.reverse)

    //loop function
  case STAR(r) =>
    val valid = shifts(Set(in), STAR(r), s) & shiftsBack(Set(out), STAR(r), s)
    def go(cur: Int): List[Val] =
      if cur == out then Nil
      else
        val p = (shifts(Set(cur), r, s) & valid).filter(_ > cur).max
        mkval(r, cur, p, s) :: go(p)
    Stars(go(in))


      */
type main = mainargs.main; private def shifts09B_scala_millScriptMainSelf = this; object MillScriptMain_shifts09B_scala { def main(args: Array[String]): Unit = this.getClass.getMethods.find(m => m.getName == "main" && m.getParameters.map(_.getType) == Seq(classOf[Array[String]]) && m.getReturnType == classOf[Unit]) match{ case Some(m) => m.invoke(shifts09B_scala_millScriptMainSelf, args); case None => mainargs.Parser(shifts09B_scala_millScriptMainSelf).runOrExit(args) }}