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

// array at each regex node.
import Rexp._

//shifting m and trace coupled. 
enum RexpA(val array: Array[Bits]) {
  case ZEROA(a: Array[Bits]) extends RexpA(a)
  case ONEA(a: Array[Bits]) extends RexpA(a)
  case CHARA(c: Char, a: Array[Bits]) extends RexpA(a)
  case ALTA(r1: RexpA, r2: RexpA, a: Array[Bits]) extends RexpA(a)
  case SEQA(r1: RexpA, r2: RexpA, a: Array[Bits]) extends RexpA(a)
  //case STARA(r: RexpA, a: Array[Bits]) extends RexpA(a)
  case STARA(r: RexpA, a: Array[Bits], i: Array[Bits]) extends RexpA(a)
  case STARCA(r: RexpA, a: Array[Bits], i: Array[Bits]) extends RexpA(a)
  case NTIMESA(r: RexpA, n: Int, a: Array[Bits]) extends RexpA(a)
  case OPTIONALA(r: RexpA, a: Array[Bits]) extends RexpA(a)
  case ANDA(r1: RexpA, r2: RexpA, a: Array[Bits]) extends RexpA(a)
  case NOTA(r: RexpA, a: Array[Bits]) extends RexpA(a)
}

import RexpA._

def clear(a: Array[Bits]): Unit =
  for (i <- a.indices) a(i) = null 

def internalizeA(r: Rexp, n: Int): RexpA = {
  def newArray = new Array[Bits](n + 1)
  r match {
    case ZERO => ZEROA(newArray)
    case ONE => ONEA(newArray)
    case CHAR(c) => CHARA(c, newArray)
    case ALT(r1, r2) => ALTA(internalizeA(r1, n), internalizeA(r2, n), newArray)
    case SEQ(r1, r2) => SEQA(internalizeA(r1, n), internalizeA(r2, n), newArray)
    //case STAR(r) => STARA(internalizeA(r, n), newArray)
    case STAR(r) => STARA(internalizeA(r, n), newArray, newArray)
    case NTIMES(r, n1) => NTIMESA(internalizeA(r, n), n1, newArray)
    case OPTIONAL(r) => OPTIONALA(internalizeA(r, n), newArray)
    case AND(r1, r2) => ANDA(internalizeA(r1, n), internalizeA(r2, n), newArray)
    case NOT(r) => NOTA(internalizeA(r, n), newArray)
  }
}

def mat(ar: RexpA, s: String): Array[Bits] = {
  def shifts2(ms: List[Int], in: Array[Bits], r: RexpA): List[Int] = 
    //println(s"ms= $ms")
    r match {
    case ZEROA(a) =>
      clear(a)
      Nil

    case ONEA(a) =>
      for (i <- in.indices) a(i) = in(i)
      ms
    case CHARA(c, a) =>
      clear(a)
      for (m <- ms if m < s.length && s(m) == c) yield {
        a(m + 1) = in(m)
        m + 1
      }
    case ALTA(r1, r2, a) =>
      clear(a)
      for (m <- ms) a(m) = Bit.Z :: in(m)
      val ms1 = shifts2(ms, a, r1)

      clear(a)
      for (m <- ms) a(m) = Bit.S :: in(m)
      val ms2 = shifts2(ms, a, r2)

      clear(a)
      for (m <- ms1) if (a(m) == null) a(m) = r1.array(m)
      for (m <- ms2) if (a(m) == null) a(m) = r2.array(m)

      (ms1 ::: ms2).distinct

    case SEQA(r1, r2, a) =>
      clear(a)
      val ms1 = shifts2(ms, in, r1).sorted.reverse
      var out: List[Int] = Nil

      for (m <- ms1) {
        val ms2 = shifts2(List(m), r1.array, r2)
        for (b <- ms2) {
          if (a(b) == null) {
            a(b) = r2.array(b)
            out = b :: out
          }
        }
      }
      out.reverse

    case star @ STARA(r, a, i) =>
      for (m <- ms) a(m) = Bit.S :: in(m)
      var ms1: List[Int] = Nil
      for (m <- ms) {
        i(m) = Bit.Z :: in(m)
        val ms2 = shifts2(List(m), i, r).sorted.reverse
        for (b <- ms2)
          if (a(b) == null) {
            a(b) = Bit.S :: r.array(b)
            i(b) = r.array(b)
            ms1 = b :: ms1
          }
        i(m) = null
      }

      ms1 = ms1.reverse

      if (ms1.isEmpty) ms
      else (ms ::: shifts2(ms1, i, star)).distinct



  }

  val start = new Array[Bits](s.length + 1)
  start(0) = Nil

  shifts2(List(0), start, ar)
  ar.array
}


def matcher(r: Rexp, s: String): Boolean ={
  mat(internalizeA(r, s.length),s)(s.length) != null
}
  
def lex(r: Rexp, s: String): Bits =
  mat(internalizeA(r, s.length), s)(s.length) match {
    case null => throw new Exception("no match")
    case bs => bs.reverse
}

def lexer(r: Rexp, s: String): Val =
  decode(r, lex(r, s))

val reg = %( %("a") | %("aa") | %("aaa") | %("aaaa") | %("aaaaa") ) 
val s   = "a" * 5400

import scala.collection.parallel.CollectionConverters._
import scala.util._

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
        println(s"number is: $i")
      }
    }
  }
}



def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

//def main(args: Array[String]): Unit = testall()
@main def runTestAll(): Unit = testall()

/*
def mat(ar: RexpA, s: String): Array[Bits] = {

  def shifts2(ms: Set[Int], in: Array[Bits], r: RexpA): Set[Int] = r match {
    case ZEROA(a) =>
      clear(a)
      Set()
    case ONEA(a) =>
      for (i <- in.indices) a(i) = in(i)
      ms
    case CHARA(c, a) =>
      clear(a)
      for (m <- ms if m < s.length && s(m) == c) yield {
        a(m + 1) = in(m)
        m + 1
      }
    case ALTA(r1, r2, a) =>
      clear(a)
      for (m <- ms) a(m) = Bit.Z :: in(m)
      val ms1 = shifts2(ms, a, r1)
      clear(a)
      for (m <- ms) a(m) = Bit.S :: in(m)
      val ms2 = shifts2(ms, a, r2)
      clear(a)
      for (b <- ms1) if (a(b) == null) a(b) = r1.array(b)
      for (b <- ms2) if (a(b) == null) a(b) = r2.array(b)
      ms1 ++ ms2

    case SEQA(r1, r2, a) =>
      clear(a)
      val ms1 = shifts2(ms, in, r1).toList.sorted.reverse
      var out: Set[Int] = Set()
      for (m <- ms1) {
        val ms2 = shifts2(Set(m), r1.array, r2)
        for (b <- ms2) {
          if (a(b) == null) {
            a(b) = r2.array(b)
            out += b
          }
        }
      }
      out
     case STARA(r, a, i) =>
        clear(a)
        clear(i)
        for (m <- ms) a(m) = Bit.S :: in(m) 
        for (m <- ms) i(m) = Bit.Z :: in(m)
        val ms1 = shifts2(ms, i, r).diff(ms)
        if (ms1 == Set()) ms
        else {
          val starNext = STARA(r, new Array[Bits](s.length + 1), new Array[Bits](s.length + 1))
          val msStar = shifts2(ms1, r.array, starNext)
          for (m <- msStar) if (a(m) == null) a(m) = starNext.array(m)
          ms ++ msStar
        } 

  }

  val start = new Array[Bits](s.length + 1)
  start(0) = Nil
  val ms = shifts2(Set(0), start, ar)
  ar.array
}




    case ALTA(r1, r2, a) =>
      clear(a)
      for (m <- ms) a(m) = in(m) :+ Bit.Z
      val ms1 = shifts2(ms, a, r1)
      clear(a)
      for (m <- ms) a(m) = in(m) :+ Bit.S
      val ms2 = shifts2(ms, a, r2)
      clear(a)
      for (b <- ms1) if (a(b) == null) a(b) = r1.array(b)
      for (b <- ms2) if (a(b) == null) a(b) = r2.array(b)
      ms1 ++ ms2
    

    case STARA(r, a, i) =>
      clear(a)

      @annotation.tailrec
      def rec(ms: Set[Int], in1: Array[Bits], out: Set[Int]): Set[Int] = {
        for (m <- ms)
          if (a(m) == null) a(m) = Bit.S :: in1(m)

        clear(i)

        for (m <- ms)
          i(m) = Bit.Z :: in1(m)

        val ms1 = shifts2(ms, i, r)
        val fresh = ms1.filter(m => a(m) == null)

        for (m <- fresh)
          a(m) = Bit.S :: r.array(m)

        if (fresh.isEmpty) out ++ fresh
        else rec(fresh, r.array, out ++ fresh)
      }

      rec(ms, in, ms)

  
/*     case STARA(r, a, i) => // first time shifting in this star, consectuive times will be done by STARCA.
      clear(a)
      shifts2(ms, in, STARCA(r, a, i))

    case star @ STARCA(r, a, i) =>
      clear(i)

      for (m <- ms) {
        if (a(m) == null) a(m) = Bit.S :: in(m) // zero repitions bits.
        i(m) = Bit.Z :: in(m) // next iteration bits
      }

      val ms1 = shifts2(ms, i, r).filter(m => a(m) == null) // == ms1.diff(ms)

      if (ms1.isEmpty) ms
      else ms ::: shifts2(ms1, r.array, star) 

    case STARA(r, a, i) =>
      clear(a)
      clear(i)
      for (m <- ms) a(m) = Bit.S :: in(m)
      for (m <- ms) i(m) = Bit.Z :: in(m)
      val ms1 = shifts2(ms, i, r).filterNot(ms.contains)
      if (ms1.isEmpty) ms
      else {
        val starNext = STARA( r, new Array[Bits](s.length + 1), new Array[Bits](s.length + 1))
        val msStar = shifts2(ms1.sorted.reverse, r.array, starNext)
        for (m <- msStar) if (a(m) == null) a(m) = starNext.array(m)
        (ms ::: msStar).distinct

      }
    */


*/