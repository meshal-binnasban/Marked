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

type Marks = Set[String]

// shifts2 function 
def shifts2(ms: Marks, r: Rexp) : Marks = r match {
  case ZERO => Set()
  case ONE => ms
  case CHAR(c) => for (m <- ms; if m != "" && m.head == c) yield m.tail
  case ALT(r1, r2) => shifts2(ms, r1) ++ shifts2(ms, r2)
  case SEQ(r1, r2) => shifts2(shifts2(ms, r1), r2)
  case STAR(r) => {
    val ms1 = shifts2(ms, r).diff(ms)
    if (ms1 == Set()) ms else ms ++ shifts2(ms1, STAR(r)) 
  }
  case NTIMES(r1, n) =>
    if (n == 0) ms 
    else shifts2(shifts2(ms, r1), NTIMES(r1, n - 1))
  case NOT(r1) =>
    ms.flatMap(s => s.tails.toSet.diff(shifts2(Set(s), r1)))  
}

// all values
def vals(s: String, r: Rexp): Set[(Val, String)] = r match {
  case ZERO => Set()
  case ONE  => Set[(Val, String)]((Empty, s))
  case CHAR(c) =>
    if (s != "" && s.head == c) Set[(Val, String)]((Chr(c), s.tail))
    else Set()
  case ALT(r1, r2) =>
    vals(s, r1).map { case (v, s1) => (Left(v): Val, s1) } ++
    vals(s, r2).map { case (v, s1) => (Right(v): Val, s1) }
  case SEQ(r1, r2) =>
    for ((v1, s1) <- vals(s, r1); (v2, s2) <- vals(s1, r2))
      yield (Sequ(v1, v2): Val, s2)
  case STAR(r0) =>
    // each iteration must consume >=1 char (s1 is a suffix of s),
    // mirroring shifts2's `.diff(ms)` guard and inj's non-empty stars
    def go(s: String): Set[(List[Val], String)] =
      Set[(List[Val], String)]((Nil, s)) ++
      (for ((v, s1) <- vals(s, r0); if s1.length < s.length;
            (vs, s2) <- go(s1)) yield (v :: vs, s2))
    go(s).map { case (vs, s1) => (Stars(vs): Val, s1) }
}

// inj-analog for shifts2: given the input mark mIn and output mark mOut of a
// match by r, reconstruct the value.
def mkval(r: Rexp, mIn: String, mOut: String): Val = r match {
  case ONE     => Empty
  case CHAR(c) => Chr(c)
  case ALT(r1, r2) =>
    if (shifts2(Set(mIn), r1) contains mOut) Left(mkval(r1, mIn, mOut))
    else                                     Right(mkval(r2, mIn, mOut))

  case SEQ(r1, r2) =>
    val mid = shifts2(Set(mIn), r1)
                .filter(m => shifts2(Set(m), r2) contains mOut)
                .minBy(_.length)                       // longest r1 match  (POSIX)
    Sequ(mkval(r1, mIn, mid), mkval(r2, mid, mOut))
  
  case STAR(r0) =>
    if (mIn == mOut) Stars(Nil)
    else {
      val mid = shifts2(Set(mIn), r0)
                  .filter(m => m.length < mIn.length &&
                               (shifts2(Set(m), STAR(r0)) contains mOut))
                  .minBy(_.length)                     // longest first iteration (POSIX)
      mkval(STAR(r0), mid, mOut) match {
        case Stars(rest) => Stars(mkval(r0, mIn, mid) :: rest)
      }
    }
  case ZERO => throw new Exception("mkval: unreachable")
}


def lexShift(r: Rexp, s: String): Option[Val] =
  if (shifts2(Set(s), r) contains "") Some(mkval(r, s, "")) else None

// the main matching function 
def mat(r: Rexp, s: String) : Marks = 
  shifts2(Set(s), r)

def matcher(r: Rexp, s: String) : Boolean = {
  if (s == "") nullable(r)
  else mat(r, s).exists(_ == "")
}

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

/*
def mkstar(n: Int) = STAR("a" * n)
def mkalts(n: Int) = {
  (for (i <- (1 to n).toList) yield mkstar(i)).reduceLeft(ALT.apply)
} 

*/

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
  (2, cs => ALT(cs(0), cs(1))),
  (2, cs => SEQ(cs(0), cs(1)))
), maxIndex = 500_000_000L)


//testing function using pushGen
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
        println(s"Marks: $vm")
        println(s"Derivative: $vb")
        sys.exit(1)
      }
    }
  }

  println(s"No mismatches found in the first $i regexes.")
}


//testing function using decode parallel
def testallDP(maxIndex: Long): Unit = {
  val alphabet = LazyList('a', 'b', 'c')

  for (i <- (0L to maxIndex).par) {
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
        println(s"Index: $i")
        println(s"Regex: $r")
        println(s"String: $s")
        println(s"lexShift: $vm")
        println(s"Derivative: $vb")
        System.exit(1)
      }
    }
  }

  println()
  println(s"No mismatches found up to $maxIndex.")
}
//testing function using decode sequential
def testallDS(maxIndex: Long): Unit = {
  val alphabet = LazyList('a', 'b', 'c')

  for (i <- (0L to maxIndex)) {
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
        println(s"Index: $i")
        println(s"Regex: $r")
        println(s"String: $s")
        println(s"lexShift: $vm")
        println(s"Derivative: $vb")
        System.exit(1)
      }
    }
  }

  println()
  println(s"No mismatches found up to $maxIndex.")
}

//main funciton for testing using pushGen with time report
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

//main funciton for testing using decode parallel with time report
@main
def measureDP(maxRegexes: Int): Unit = {
  val t = time_needed(1, testallDP(maxRegexes))

  println()
  println("========================================")
  println("Method: testallDP")
  println("Enumeration: decode parallel")
  println(s"Number of regexes: $maxRegexes")
  println(s"Time: $t seconds")
  println("Result: no mismatch")
  println("========================================")
}

//main funciton for testing using decode sequential with time report
@main
def measureDS(maxRegexes: Int): Unit = {
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







