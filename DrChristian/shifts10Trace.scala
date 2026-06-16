//| mainClass: Main
//| scalaVersion: 3.8.3
//| scalacOptions: ["-deprecation", "-feature", "-language:implicitConversions"]
//| mvnDeps: 
//|   - org.scala-lang:scala3-library_3:3.8.3
//|   - org.scala-lang.modules::scala-parallel-collections:1.2.0
//| moduleDeps: 
//|   - rexp.scala
//|   - enumerate.scala
//|   - regenerate.scala
//|   - re_bitrev3.scala
//> using scala 3.8.3
//> using dep org.scala-lang.modules::scala-parallel-collections:1.2.0
//> using file rexp.scala
//> using file enumerate.scala
//> using file regenerate.scala
//> using file re_bitrev3.scala

// single marks
import Rexp._

type Mark = (Int, Bits)
type Marks = List[Mark]

def mat(r: Rexp, s: String): Array[Bits] = {
  val tArray = new Array[Bits](s.length + 1)
  //var counter = 0
  def shifts2(ms: Marks, r: Rexp): Marks = 
    //counter += 1
    r match {
    case ZERO => Nil
    case ONE => ms
    case CHAR(c) =>
      for ((m, bs) <- ms if m < s.length && s(m) == c) yield {
        if (tArray(m + 1) == null) tArray(m + 1) = bs // might not be needed, only for char only regex?
        (m + 1, bs)
      }
    case ALT(r1, r2) =>
      val mss = ms.flatMap { case (m, bs) =>
        val ms1 = shifts2(List((m, Bit.Z :: bs)), r1)
        val ms2 = shifts2(List((m, Bit.S :: bs)), r2)
          .filterNot { case (b, _) => ms1.exists(_._1 == b) }
        ms1 ::: ms2 }.distinctBy(_._1) // collapse to first mark reached, of ms1 or left.
      
      for ((m, bs) <- mss) tArray(m) = bs
      mss

    case SEQ(r1, r2) =>
      val ms1 = shifts2(ms, r1).sortBy(_._1).reverse
      val ms2 = shifts2(ms1, r2)
      
      for ((m, bs) <- ms2.sortBy(_._1).reverse)
        tArray(m) = bs
      ms2
    
    case STAR(r) =>
      val ms0 = ms.map { case (m, bs) => (m, Bit.S :: bs) } // zero repetitions
      val msi = ms.map { case (m, bs) => (m, Bit.Z :: bs) } //prepare marks for one iteration
      val ms1 = shifts2(msi, r).filterNot { case (m, _) => ms.exists(_._1 == m) }
        .sortBy(_._1).reverse // marks for one iteration with ms1.diff(ms) equivalent, sorted furthest first. 

      if (ms1.isEmpty)
        for ((m, bs) <- ms0) tArray(m) = bs
        ms0 
      else 
        val ms2= (ms0 ::: shifts2(ms1, STAR(r))).distinctBy(_._1)
        for ((m, bs) <- ms2) tArray(m) = bs
        ms2

    case NTIMES(r, 0) =>
      val mss = ms.map { case (m, bs) => (m, Bit.S :: bs) }
      for ((m, bs) <- mss) tArray(m) = bs
      mss

    case NTIMES(r, n) =>
      val msi = ms.map { case (m, bs) => (m, Bit.Z :: bs) }
      val ms1 = shifts2(msi, r).sortBy(_._1).reverse
      val ms2 = shifts2(ms1, NTIMES(r, n - 1))
      for ((m, bs) <- ms2) tArray(m) = bs
      ms2
  }

  shifts2(List((0, Nil)), r)
  //println(s"Total shifts: $counter")
  tArray
}

def matcher(r: Rexp, s: String): Boolean =
  if (s == "") nullable(r)
  else mat(r, s)(s.length) != null

def lex(r: Rexp, s: String): Bits =
  if (s == "") {
    if (nullable(r)) mkepsBits(r)
    else throw new Exception("no match")
  } else {
    mat(r, s)(s.length) match {
      case null => throw new Exception("no match")
      case bs   => bs.reverse
    }
  }

def lexer(r: Rexp, s: String): Val =
  decode(r, lex(r, s))
  
//end of //shifting m and trace coupled. 







// tests for trace
def tests(): Unit =
  List[(Rexp, String)](
    ((ONE | "a") ~ ("ab" | "b"), "ab"),
    ((ONE | "c") ~ (("c" ~ "c") | "c"), "cc"),
    ("aa" | ("a" ~ (ONE ~ "a")), "aa"),
    ((ONE ~ "a") | ("a" ~ ONE), "a"),
    ((("a" | "b") | "b"), "b"),
    ("a" | ("ab" | "ba"), "ab"),
    ((("a" | "ab") ~ ("b" | ONE)), "ab"),
    ("abc", "abc"),
    ((("a" | ("a" ~ "a")) ~ ("a" | ("a" ~ "a"))), "aaa"),
    ((((("a" ~ "a") | "a") ~ ("a" | ("a" ~ "a")))), "aaa"),
    ((("a" | ("a" ~ "a")) ~ ("a" | ("a" ~ "a"))), "aaa"),
    (((("a" | "c") ~ ("c" ~ "b")) | (((ZERO ~ ONE) ~ ONE))), "acb"),
    (ONE | "a", "a")
  ).zipWithIndex.foreach { case ((r, s), i) =>
    val marks = lex(r, s)
    val der = blex_simp(internalise(r), s.toList).reverse

    println(s"Test ${i + 1}")
    println(s"Regex: $r")
    println(s"Input: $s")
    println(s"Marks: ${marks.mkString("(", ",", ")")}")
    println(s"Derivative: ${der.mkString("(", ",", ")")}")
    println(s"Equal: ${marks == der}")
    println()
  }

def testsStar(): Unit =
  List[(Rexp, String)](
    (((("b" ~ ONE) | %("b")) ~ %("b" | "c")), "bbc"),
    ((%(ONE) ~ "a"), "a"),
    (("a" | %("a")), "a"),
    ((ONE | %("a")), "a"),
    (%("a" | "aa"), "aaa"),
    ((%("a") | %("aa")), "aa"),
    (((ONE | "a") ~ %("a")), "a"),
    (((("a" ~ ONE) | (ONE ~ "a")) ~ %("a")), "aaaaaaaaa"),
    (%("a" | "aa"), "aaa"),
    ((%("a" | "b")), "aba"),
    (("a" | ONE) ~ %("a"), ""),
    ((("a" | ONE) ~ "a") ~ %("a"), "aaa"),
    (("b" ~ ONE | %("a")) ~ %("a" | "b"), "aab"),
    (%("b" | %("a")), "bab")
  ).zipWithIndex.foreach { case ((r, s), i) =>
    val marks = lex(r, s)
    val der = blex_simp(internalise(r), s.toList).reverse

    println(s"Test ${i + 1}")
    println(s"Regex: $r")
    println(s"Input: $s")
    println(s"Marks: ${marks.mkString("(", ",", ")")}")
    println(s"Derivative: ${der.mkString("(", ",", ")")}")
    println(s"Equal: ${marks == der}")
    println()
  }

def testsStarTimed(i: Int = 1000): Unit = {
  val examples = List[(Rexp, String)](
    (((("b" ~ ONE) | %("b")) ~ %("b" | "c")), "bbc"),
    ((%(ONE) ~ "a"), "a"),
    (("a" | %("a")), "a"),
    ((ONE | %("a")), "a"),
    (%("a" | "aa"), "aaa"),
    ((%("a") | %("aa")), "aa"),
    (((ONE | "a") ~ %("a")), "a"),
    (((("a" ~ ONE) | (ONE ~ "a")) ~ %("a")), "aaaaaaaaa"),
    (%("a" | "aa"), "aaa"),
    ((%("a" | "b")), "aba"),
    (("a" | ONE) ~ %("a"), ""),
    ((("a" | ONE) ~ "a") ~ %("a"), "aaa"),
    (("b" ~ ONE | %("a")) ~ %("a" | "b"), "aab"),
    (%("b" | %("a")), "bab")
  )

  var totalArray = 0.0
  var totalDer = 0.0
  var arrayWins = 0
  var derWins = 0
  var ties = 0
  var equal = 0

  examples.zipWithIndex.foreach { case ((r, s), n) =>
    val marks = lex(r, s)
    val der = blex_simp(internalise(r), s.toList).reverse

    val arrayTime = time_needed(i, lex(r, s))
    val derTime = time_needed(i, blex_simp(internalise(r), s.toList).reverse)

    totalArray += arrayTime
    totalDer += derTime

    val faster =
      if (arrayTime < derTime) {
        arrayWins += 1
        s"Array faster by ${derTime / arrayTime}x"
      } else if (derTime < arrayTime) {
        derWins += 1
        s"Derivative faster by ${arrayTime / derTime}x"
      } else {
        ties += 1
        "Same time"
      }
    if (marks == der) equal += 1

    println(s"Test ${n + 1}")
    println(s"Regex: $r")
    println(s"Input: $s")
    println(s"Marks: ${marks.mkString("(", ",", ")")}")
    println(s"Derivative: ${der.mkString("(", ",", ")")}")
    println(s"Equal: ${marks == der}")
    println(s"Array average time: $arrayTime")
    println(s"Derivative average time: $derTime")
    println(s"Faster: $faster")
    println()
  }

  val avgArray = totalArray / examples.length
  val avgDer = totalDer / examples.length

  println("--- Summary ---")
  println(s"Number of examples: ${examples.length}")
  println(s"Array faster examples: $arrayWins")
  println(s"Derivative faster examples: $derWins")
  println(s"Ties: $ties")
  println(s"Average Array time: $avgArray")
  println(s"Average Derivative time: $avgDer")
  println(s"Equal results: $equal out of ${examples.length}")

  if (arrayWins > derWins)
    println(s"Overall by examples: Array faster in more examples: $arrayWins vs $derWins")
  else if (derWins > arrayWins)
    println(s"Overall by examples: Derivative faster in more examples: $derWins vs $arrayWins")
  else
    println(s"Overall by examples: Tie: $arrayWins vs $derWins")
}

def testsNT(): Unit = {
  val examples = List[(Rexp, String)](
    (NTIMES("a", 0), ""),
    (NTIMES("a", 1), "a"),
    (NTIMES("a", 3), "aaa"),
    (NTIMES("ab", 2), "abab"),
    (NTIMES(ONE, 3), ""),
    (NTIMES("a" | "b", 3), "aba"),
    (NTIMES("a" | "aa", 2), "aaa"),
    (NTIMES("a" | "aa", 3), "aaaa"),
    (NTIMES(ONE | "a", 2), "a"),
    (NTIMES(ONE | "a", 3), "aa"),
    (NTIMES(NTIMES("a", 2), 2), "aaaa"),
    (NTIMES("a", 2) ~ "b", "aab"),
    ("b" ~ NTIMES("a", 2), "baa"),
    (NTIMES("a", 2) ~ NTIMES("b", 2), "aabb"),
    ((ONE | "a") ~ NTIMES("a", 2), "aaa"),
    (NTIMES("a", 2) | "aa", "aa"),
    ("aa" | NTIMES("a", 2), "aa"),
    (NTIMES("a" | ("a" ~ "a"), 2), "aaa"),
    (NTIMES(%("a"), 2), "aaa"),
    (%(NTIMES("a", 2)), "aaaa"),
    (NTIMES(%("a"),3),"aa"),
    (NTIMES(("a"|"aa"),2), "aa")
  )

  var equalCount = 0
  var arrayWins = 0
  var derivativeWins = 0
  var ties = 0
  var totalArray = 0.0
  var totalDerivative = 0.0

  examples.zipWithIndex.foreach { case ((r, s), n) =>
    val ir = internalise(r)
    val cs = s.toList
    val marks = lex(r, s)
    val derivative = blex_simp(ir, cs).reverse
    val equal = marks == derivative


    if (equal) equalCount += 1

    println(s"Test ${n + 1}")
    println(s"Regex: $r")
    println(s"Input: $s")
    println(s"Marks: ${marks.mkString("(", ",", ")")}")
    println(s"Derivative: ${derivative.mkString("(", ",", ")")}")
    println(s"Equal: $equal")
    println()
  }

  println(s"Number of examples: ${examples.length}")
  println(s"Equal examples: $equalCount / ${examples.length}")

}

def testsNTTimed(i: Int = 1000): Unit = {
  val examples = List[(Rexp, String)](
    (NTIMES("a", 0), ""),
    (NTIMES("a", 1), "a"),
    (NTIMES("a", 3), "aaa"),
    (NTIMES("ab", 2), "abab"),
    (NTIMES(ONE, 3), ""),
    (NTIMES("a" | "b", 3), "aba"),
    (NTIMES("a" | "aa", 2), "aaa"),
    (NTIMES("a" | "aa", 3), "aaaa"),
    (NTIMES(ONE | "a", 2), "a"),
    (NTIMES(ONE | "a", 3), "aa"),
    (NTIMES(NTIMES("a", 2), 2), "aaaa"),
    (NTIMES("a", 2) ~ "b", "aab"),
    ("b" ~ NTIMES("a", 2), "baa"),
    (NTIMES("a", 2) ~ NTIMES("b", 2), "aabb"),
    ((ONE | "a") ~ NTIMES("a", 2), "aaa"),
    (NTIMES("a", 2) | "aa", "aa"),
    ("aa" | NTIMES("a", 2), "aa"),
    (NTIMES("a" | ("a" ~ "a"), 2), "aaa"),
    (NTIMES(%("a"), 2), "aaa"),
    (%(NTIMES("a", 2)), "aaaa")
  )

  var equalCount = 0
  var arrayWins = 0
  var derivativeWins = 0
  var ties = 0
  var totalArray = 0.0
  var totalDerivative = 0.0

  examples.zipWithIndex.foreach { case ((r, s), n) =>
    val ir = internalise(r)
    val cs = s.toList
    val marks = lex(r, s)
    val derivative = blex_simp(ir, cs).reverse
    val equal = marks == derivative
    val arrayTime = time_needed(i, lex(r, s))
    val derivativeTime = time_needed(i, blex_simp(ir, cs).reverse)

    if (equal) equalCount += 1

    totalArray += arrayTime
    totalDerivative += derivativeTime

    println(s"Test ${n + 1}")
    println(s"Regex: $r")
    println(s"Input: $s")
    println(s"Marks: ${marks.mkString("(", ",", ")")}")
    println(s"Derivative: ${derivative.mkString("(", ",", ")")}")
    println(s"Equal: $equal")
    println(s"Array time: $arrayTime")
    println(s"Derivative time: $derivativeTime")

    if (arrayTime < derivativeTime) {
      arrayWins += 1
      println(s"Faster: Array by ${derivativeTime / arrayTime}")
    } else if (derivativeTime < arrayTime) {
      derivativeWins += 1
      println(s"Faster: Derivative by ${arrayTime / derivativeTime}")
    } else {
      ties += 1
      println("Faster: Tie")
    }

    println()
  }

  val averageArray = totalArray / examples.length
  val averageDerivative = totalDerivative / examples.length

  println(s"Number of examples: ${examples.length}")
  println(s"Equal examples: $equalCount / ${examples.length}")
  println(s"Array faster examples: $arrayWins")
  println(s"Derivative faster examples: $derivativeWins")
  println(s"Ties: $ties")
  println(s"Average Array time: $averageArray")
  println(s"Average Derivative time: $averageDerivative")

  if (arrayWins > derivativeWins)
    println(s"Overall by examples: Array faster in more examples: $arrayWins vs $derivativeWins")
  else if (derivativeWins > arrayWins)
    println(s"Overall by examples: Derivative faster in more examples: $derivativeWins vs $arrayWins")
  else
    println(s"Overall by examples: Tie: $arrayWins vs $derivativeWins")

  if (averageArray < averageDerivative)
    println(s"Overall by average time: Array faster by ${averageDerivative / averageArray}")
  else if (averageDerivative < averageArray)
    println(s"Overall by average time: Derivative faster by ${averageArray / averageDerivative}")
  else
    println("Overall by average time: Tie")
}

import scala.collection.parallel.CollectionConverters._
import scala.util._

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
    (2, cs => SEQ(cs(0), cs(1))),
   // (1, cs => NOT(cs(0))),
   // (1, cs => NTIMES(cs(0), 3))
  )

  val alphabet = LazyList('a', 'b', 'c')

  for (i <- (0L to 1_000_000_000L)) {
    if (i % 100_000L == 0L) { print("*") }

    val r = decodeRegex[Rexp](BigInt(i))

    for (s <- generate_up_to(alphabet)(10)(r).take(10) if s != "") {
      val vm = lex(r, s)
      val vb = blex_simp(internalise(r), s.toList).reverse

      if (vm != vb) {
        println(s"\nMismatch:")
        println(s"$i: $r")
        println(s"String: $s")
        println(s"Marks: ${vm.mkString("(", ",", ")")}")
        println(s"Derivative: ${vb.mkString("(", ",", ")")}")
      }
    }
  }
}
//end of // tests for trace

//def main(args: Array[String]): Unit = testall()
@main def runTestAll(): Unit = testall()

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

val reg = %( %("a") | %("aa") | %("aaa") | %("aaaa") | %("aaaaa") ) 
val s   = "a" * 5400
