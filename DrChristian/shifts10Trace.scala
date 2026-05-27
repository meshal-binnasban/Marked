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

type Trace = List[Int]
type Mark = (Int, Trace)
type Marks = List[Mark]

//shifting m and trace coupled. 
def mat(r: Rexp, s: String): Array[Trace] = {
  val tArray = new Array[Trace](s.length + 1)
  def shifts2(m: Mark, r: Rexp): Marks = 
    r match {
    case ZERO => Nil
    case ONE => List(m)
    case CHAR(c) => 
      val (mm, bs) = m
      if (mm < s.length && s(mm) == c) List((mm + 1, bs)) else Nil
    
    case ALT(r1, r2) => 
      val (mm, bs) = m
      shifts2((mm, bs :+ 0), r1) ++ shifts2((mm, bs :+ 1), r2)

    case SEQ(r1, r2) => shifts2(m, r1).sortBy(-_._1).flatMap(m2 => shifts2(m2, r2))
    
    case STAR(r) =>
      val (mm, bs) = m
      val ms = List((mm, bs :+ 1))

      val ms1 = shifts2((mm, bs :+ 0), r).filterNot { case (m1, _) => m1 == mm }
      if (ms1.isEmpty) ms 
      else ms ++ ms1.sortBy(-_._1).flatMap(shifts2(_, STAR(r)))
    
    case NTIMES(r, n) =>
      val (mm, bs) = m
      val ms= List((mm, bs :+ 1))

      if (n == 0) ms
      else shifts2((mm, bs :+ 0), r).sortBy(-_._1).flatMap(m2 => shifts2(m2, NTIMES(r, n - 1)))

    case NOT(r) =>
      val (mm, bs) = m
      val allFromM = Range(mm, s.length + 1).toSet
      val rFromM = shifts2((mm, bs), r).map(_._1).toSet
      allFromM.diff(rFromM).toList.map(m1 => (m1, bs))

  }

  val x= shifts2((0, Nil), r)
  println(s"returned Marks: ${x.mkString("(", ",", ")")}")
  x.foreach {
    case (m, bs) => if (tArray(m) == null) tArray(m) = bs
  }

  tArray
}
def matcher(r: Rexp, s: String): Boolean = {

  if (s == "") nullable(r)
  else mat(r, s)(s.length) != null
}
// converst int into bit for comparing with derivative.
def intToBit(i: Int): Bit = i match {
  case 0 => Bit.Z
  case 1 => Bit.S
}

def lex(r: Rexp, s: String): Bits =
  s match {
    case "" => if (nullable(r)) mkepsBits(r) else throw new Exception("no match")
    case _ =>
      val trace = mat(r, s)
      if (trace(s.length) == null) throw new Exception("no match")
      else trace(s.length).map(intToBit)
}

def lexer(r: Rexp, s: String): Val =
  s match {
    case "" => if (nullable(r)) mkeps(r) else throw new Exception("no match")
    case _ =>
      val trace = mat(r, s)
      if (trace(s.length) == null) throw new Exception("no match")
      else decode(r, trace(s.length).map(intToBit))
  }
  
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

  for (i <- (0L to 1_000_000_000L).par) {
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

//experiment: shifting with commit to array.
 /* 
type Trace = List[Int]
type Mark = (Int, Trace)
type Marks = List[Mark]

def mat(r: Rexp, s: String): Array[Trace] = {
  val tArray = new Array[Trace](s.length + 1)

  def shifts2(m: Mark, r: Rexp, commit: Boolean): Marks = r match {
    case ZERO => Nil
    case ONE => List(m)

    case CHAR(c) =>
      val (mm, bs) = m
      if (mm < s.length && s(mm) == c) {
        if (commit && tArray(mm + 1) == null) tArray(mm + 1) = bs
        List((mm + 1, bs))
      } else Nil

    case ALT(r1, r2) =>
      val (mm, bs) = m
      shifts2((mm, bs :+ 0), r1, commit) ++ shifts2((mm, bs :+ 1), r2, commit)

    case SEQ(r1, r2) =>
      shifts2(m, r1, false).sortBy(-_._1).flatMap(m2 => shifts2(m2, r2, commit))

    case STAR(r) =>
      val (mm, bs) = m
      val ms = List((mm, bs :+ 1))

      val ms1 = shifts2((mm, bs :+ 0), r, false).filterNot { case (m1, _) => m1 == mm }
      if (ms1.isEmpty) ms
      else ms ++ ms1.sortBy(-_._1).flatMap(m2 => shifts2(m2, STAR(r), commit))
  }

  val x = shifts2((0, Nil), r, true)
  println(s"returned Marks: ${x.mkString("(", ",", ")")}")

  tArray
} */


//shifting m and trace seperately. 
/* 
type Marks = List[(Int, Trace)]
type Trace = List[Int]

def mat(r: Rexp, s: String): Array[Trace] = {
  val tArray = new Array[Trace](s.length + 1)
  def shifts2(m: Int, r: Rexp, bs: Trace): Marks = r match {
    case ZERO => Nil
    case ONE => List((m, bs))
    case CHAR(c) => if (m < s.length && s(m) == c) List((m + 1, bs)) else Nil
    case ALT(r1, r2) =>  shifts2(m, r1, bs :+ 0) ++ shifts2(m, r2, bs :+ 1)
    case SEQ(r1, r2) => shifts2(m, r1, bs).sortBy(-_._1).flatMap { case (m1, bs1) => shifts2(m1, r2, bs1) }
    case STAR(r) =>
      val ms=List((m, bs :+ 1)) // m with bitcode/ zero repitions

      val ms1 = shifts2(m, r, bs :+ 0).filterNot {
        case (m1, _) => m1 == m
      }
      if (ms1.isEmpty) ms
      else ms ++ ms1.sortBy(-_._1).flatMap {
        case (m1, bs1) => shifts2(m1, STAR(r), bs1)
      }

  }
  val x=shifts2(0, r, Nil)
  println(s"returned Marks: ${x.mkString("(", ",", ")")}")  
  x.foreach {
    case (m, bs) =>
    if (tArray(m) == null) tArray(m) = bs
  }
  tArray
}  
 */

//shifting m and trace coupled. 
def matArray(r: Rexp, s: String): Array[Trace] = {
  val tArray = new Array[Trace](s.length + 1)
  tArray(0) = Nil

  def shifts2(ms: Set[Int], r: Rexp): Set[Int] = r match {
    case ZERO => Set()

    case ONE => ms

    case CHAR(c) =>
      for (m <- ms if m < s.length && s(m) == c) yield {
        tArray(m + 1) = tArray(m)
        m + 1
      }

    case ALT(r1, r2) =>
      ms.flatMap { m =>
        val base = tArray(m)

        tArray(m) = base :+ 0
        val ms1 = shifts2(Set(m), r1)
        val save1 = ms1.map(b => (b, tArray(b)))

        tArray(m) = base :+ 1
        val ms2 = shifts2(Set(m), r2)

        for ((b, bs) <- save1)
          tArray(b) = bs

        tArray(m) =
          if (ms1.contains(m)) base :+ 0
          else if (ms2.contains(m)) base :+ 1
          else base

        ms1 ++ ms2
      }

    case SEQ(r1, r2) =>
      var out: Set[Int] = Set()

      for (m <- ms) {
        val base = tArray(m)
        val ms1 = shifts2(Set(m), r1).toList.sortBy(-_)

        for (m1 <- ms1) {
          val base1 = tArray(m1)
          val ms2 = shifts2(Set(m1), r2)

          if (ms2.nonEmpty) {
            out = out ++ ms2
          } else {
            if (m1 == m) tArray(m) = base
          }
        }
      }

      out

    case _ => Set()
  }

  val ms = shifts2(Set(0), r)

  println(s"returned Marks: $ms")
  tArray.zipWithIndex.foreach {
    case (null, _) => ()
    case (bs, i) => println(s"$i -> ${bs.mkString("(", ",", ")")}")
  }

  tArray
}


// sets of sets of marks.
type GMarks = Set[Set[Int]]

def matG(r: Rexp, s: String): GMarks = {
  var calls = 0        // how many times shifts2G was called
  var totalGroups = 0  // how many groups
  var totalMarks = 0   // how many individual marks in those groups

  def shifts2G(mss: GMarks, r: Rexp): GMarks = {
    calls += 1
    totalGroups += mss.size
    totalMarks += mss.toList.map(_.size).sum
    r match {

      case ZERO => mss.map(_ => Set.empty[Int])

      case ONE => mss

      case CHAR(c) =>
        mss.map { ms =>
          for (m <- ms if m < s.length && s(m) == c) yield m + 1
        }

      case ALT(r1, r2) =>
        mss.map { ms =>
          shifts2G(Set(ms), r1).flatten ++
          shifts2G(Set(ms), r2).flatten
        }

      case SEQ(r1, r2) => shifts2G(shifts2G(mss, r1), r2)

      case STAR(r) =>
        mss.map { ms =>
          val ms1 = shifts2G(Set(ms), r).flatten.diff(ms)
          if (ms1.isEmpty) ms
          else ms ++ shifts2G(Set(ms1), STAR(r)).flatten
        }
    }
  }

  val result = shifts2G(Set(Set(0)), r)

  println("--- Sets of Sets Of Marks ---")
  println(s"Number of Calls: $calls")
  println(s"Total groups: $totalGroups")
  println(s"Total marks in the groups: $totalMarks")

  result
}

def matcherG(r: Rexp, s: String): Boolean =
  matG(r, s).flatten.contains(s.length)






//set of ints with counter
def matCount(r: Rexp, s: String): Set[Int] = {
  var calls = 0
  var totalMarks = 0

  def shifts2(ms: Set[Int], r: Rexp): Set[Int] = {
    calls += 1
    totalMarks += ms.size

    r match {
      case ZERO => Set()
      case ONE => ms
      case CHAR(c) => for (m <- ms if m < s.length && s(m) == c) yield m + 1
      case ALT(r1, r2) => shifts2(ms, r1) ++ shifts2(ms, r2)
      case SEQ(r1, r2) => shifts2(shifts2(ms, r1), r2)
      case STAR(r) =>
        val ms1 = shifts2(ms, r).diff(ms)
        if (ms1.isEmpty) ms else ms ++ shifts2(ms1, STAR(r))
      case NTIMES(r, n) =>
        if (n == 0) ms else shifts2(shifts2(ms, r), NTIMES(r, n - 1))
      case NOT(r) =>
        ms.flatMap { m =>
          val allFromM = Range(m, s.length + 1).toSet
          val rFromM = shifts2(Set(m), r)
          allFromM.diff(rFromM)
        }
    }
  }
  val res = shifts2(Set(0), r)
  println("--- Sets Of Marks ---")
  println(s"Number of Calls: $calls")
  println(s"Number of marks: $totalMarks")

  res
}

def matcherCount(r: Rexp, s: String): Boolean =
  if (s == "") nullable(r)
  else matCount(r, s).contains(s.length)


//testing set of sets of marks
def testsG(): Unit =
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
    
    println(s"\n--- Test ${i + 1} ---")
    val marks = matcherCount(r, s)
    
    val marksG = matcherG(r, s)
    val marksGReturned= matG(r, s)
    val der = derMatcher(r, s)

    
    println(s"Regex: $r")
    println(s"Input: $s")
    println(s"Marks: $marks , GMarks: $marksG, Derivative: $der")
    println(s"Equal to Derivative: ${marks == der} , Equal to Gmarks: ${marksG == der}")
    println(s"Marks returned from matG: $marksGReturned")
    println(s"--- End of Test ${i + 1} ---\n")
  }

def testsStarG(): Unit =
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
    
    println(s"\n--- Test ${i + 1} ---")
    val marks = matcherCount(r, s)
    val marksG = matcherG(r, s)
    val der = derMatcher(r, s)
    val marksGReturned= matG(r, s)

    println(s"Test ${i + 1}")
    println(s"Regex: $r")
    println(s"Input: $s")
    println(s"Marks: $marks , GMarks: $marksG, Derivative: $der")
    println(s"Equal to Derivative: ${marks == der} , Equal to Gmarks: ${marksG == der}")

    println(s"Marks returned from matG: $marksGReturned")
    println(s"--- End of Test ${i + 1} ---\n")
  }

def testsNotG(): Unit =
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
    val marks = matcherCount(r, s)
    val marksG = matcherG(r, s)
    val der = derMatcher(r, s)

    
    println(s"Regex: $r")
    println(s"Input: $s")
    println(s"Marks: $marks , GMarks: $marksG, Derivative: $der")
    println(s"Equal to Derivative: ${marks == der} , Equal to Gmarks: ${marksG == der}")
    println(s"Expected: $expected")
  
    println(s"--- End of Test ${i + 1} ---\n")
  }

//end of // sets of sets of marks.
//def main(args: Array[String]): Unit = testall()
@main def runTestAll(): Unit = testall()