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

def fwd(ms: Marks, r: Rexp, s: String): Marks = r match {
  case ZERO => Set()
  case ONE => ms
  case CHAR(c) => ms.collect { case m if m < s.length && s(m) == c => m + 1} //for (m <- ms; if m < s.length && s(m) == c) yield m + 1
  case ALT(r1, r2) => fwd(ms, r1, s) ++ fwd(ms, r2, s)
  case SEQ(r1, r2) => fwd(fwd(ms, r1, s), r2, s)
  case STAR(r) =>
    val ms1 = fwd(ms, r, s).diff(ms)
    if (ms1.isEmpty) ms
    else ms ++ fwd(ms1, STAR(r), s)
}

def bwd(ms: Marks, r: Rexp, s: String): Marks = r match {
  case ZERO => Set()
  case ONE => ms
  case CHAR(c) => ms.collect { case m if m > 0 && s(m - 1) == c => m - 1} //for (m <- ms; if m < s.length && s(m) == c) yield m + 1
  case ALT(r1, r2) => bwd(ms, r1, s) ++ bwd(ms, r2, s)
  case SEQ(r1, r2) => bwd(bwd(ms, r2, s), r1, s)
  case STAR(r) =>
    val ms1 = bwd(ms, r, s).diff(ms)
    if (ms1.isEmpty) ms
    else ms ++ bwd(ms1, STAR(r), s)
}
/* 
def matcher(r: Rexp, in: Int, out: Int, s: String): Boolean =
  fwd(Set(in), r, s).contains(out) */

def matcher(r: Rexp, s: String): Boolean =
  matcher(r, 0, s.length, s)

def mkval(r: Rexp, in: Int, out: Int, s: String): Val = r match {
  case ZERO => throw new Exception("mkval: ZERO")
  case ONE => Empty
  case CHAR(c) => Chr(c)
  case ALT(r1, r2) =>
    if (fwd(Set(in), r, s).contains(out)) //matcher(r1, in, out, s)
      Left(mkval(r1, in, out, s))
    else
      Right(mkval(r2, in, out, s))
  case SEQ(r1, r2) =>
    val mid =fwd(Set(in), r1, s).intersect(bwd(Set(out), r2, s)).max
    Sequ(mkval(r1, in, mid, s), mkval(r2, mid, out, s))

  case STAR(r) =>
    if (in == out) Stars(Nil)
    else {
      val next = fwd(Set(in), r, s).intersect(bwd(Set(out), STAR(r), s))
          .filter(_ > in).max

      mkval(STAR(r), next, out, s) match {
        case Stars(vs) => Stars(mkval(r, in, next, s) :: vs)
      }
    }
}

def lexShift(r: Rexp, s: String): Option[Val] =
  if (matcher(r, s)) Some(mkval(r, 0, s.length, s))
  else None



def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

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
      //val star = sub.map(STAR(_))                  // unary
      zero ++ one ++ chr ++ alt ++ seq //++ star

given rexp_enum: Enumeration[Rexp] = Enumeration.covering(List(
  (0, _  => ONE),
  (0, _  => ZERO),
  (0, _  => CHAR('a')),
  (0, _  => CHAR('b')),
  (0, _  => CHAR('c')),
  //(1, cs => STAR(cs(0))),
  (2, cs => ALT(cs(0), cs(1))),
  (2, cs => SEQ(cs(0), cs(1)))
), maxIndex = 500_000_000L)


//testing function using pushGen
def testallS(maxRegexes: Int): Unit = {
  val alphabet = LazyList('a', 'b', 'c')
  var i = 0
  rexps(4).take(maxRegexes).foreach { r =>
    i += 1
    if (i % 100_000 == 0)print("*")
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
def measureS(maxRegexes: Int): Unit = {
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