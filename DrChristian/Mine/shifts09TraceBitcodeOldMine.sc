import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.re_bitrev3

import scala.collection.parallel.ForkJoinTaskSupport
import java.util.concurrent.ForkJoinPool
import $ivy.`org.scala-lang.modules::scala-parallel-collections:1.0.4`
import scala.collection.parallel.CollectionConverters._

type Trace = List[Int]
type Marks = List[Int]

def shifts(ms: Marks, trace: Array[Trace], s: String, r: Rexp): (Marks, Array[Trace]) = {
  val n = s.length
  r match {
    case ZERO => (Nil, new Array[Trace](n + 1))
    case ONE => (ms, trace)
    case CHAR(c) =>
      val outTrace = new Array[Trace](n + 1)
      var mss: List[Int] = Nil
      for (m <- ms) {
        if (m < n && s(m) == c) {
          outTrace(m + 1) = trace(m)
          mss = (m + 1) :: mss
        }
      }
      (mss.reverse, outTrace)
    case ALT(r1, r2) =>
      val outTrace = new Array[Trace](n + 1)
      val (ms1, tr1) = shifts(ms, trace, s, r1)
      val (ms2, tr2) = shifts(ms, trace, s, r2)

      for (b <- ms1) outTrace(b) = 0 :: tr1(b)
      for (b <- ms2) {
        if (outTrace(b) == null) {
          outTrace(b) = 1 :: tr2(b)
        }
      }
      ((ms1 ::: ms2).distinct.sorted, outTrace)

    case SEQ(r1, r2) =>
      val outTrace = new Array[Trace](n + 1) // final traces for SEQ
      var mss: List[Int] = Nil

      val (ms1, tr1) = shifts(ms, trace, s, r1) // shift through r1 first

      val traceR2 = new Array[Trace](n + 1)
      for (m <- ms1.reverse) { // try furthest splits first
        traceR2(m) = Nil
        val (ms2, tr2) = shifts(List(m), traceR2, s, r2)

        for (b <- ms2) {
          if (outTrace(b) == null) { // record first winner at b 
            outTrace(b) = tr1(m) ++ tr2(b)
            mss = b :: mss
          }
        }
        traceR2(m) = null
      }
      (mss.sorted, outTrace) // reverse to keep ascending order of marks

    case STAR(r) =>

      val outTrace = new Array[Trace](n + 1) // final traces for STAR
      for (m <- ms) outTrace(m) = trace(m) ++ List(1) // zero repetitions: keep mark and add closing bit 1

      val nextTrace = new Array[Trace](n + 1) // traces for one more iteration
      var mss: List[Int] = Nil

      val traceR = new Array[Trace](n + 1)

      for (start <- ms) {
        traceR(start) = Nil
        val (ms1, tr1) = shifts(List(start), traceR, s, r)

        for (b <- ms1.reverse) {
          if (outTrace(b) == null && nextTrace(b) == null) { // avoid shifting positions already reached
            nextTrace(b) = trace(start) ++ (0 :: tr1(b)) // add 0 for a new iteration
            mss = b :: mss
          }
        }
        traceR(start) = null
      }

      if (mss.isEmpty) {
        (ms, outTrace)
      } else {
        val (msStar, trStar) = shifts(mss.reverse, nextTrace, s, STAR(r)) // continue STAR from new marks

        for (b <- msStar) {
          if (outTrace(b) == null) {
            outTrace(b) = trStar(b)
          }
        }
        ((ms ::: msStar).distinct.sorted, outTrace)
      }

    case AND(_, _) => (Nil, new Array[Trace](n + 1)) // not yet implemented
    case NTIMES(_, _) => (Nil, new Array[Trace](n + 1))// not yet implemented
  }
}

def back(r: Rexp, s: String, tr: Trace): Val =
  back_aux(r, s, tr) match {
    case (v, "", Nil) => v
    case _ => throw new Exception("Not decodable")
  }

def back_aux(r: Rexp, s: String, tr: Trace): (Val, String, Trace) =
  (r: @unchecked) match {
    case ONE => (Empty, s, tr)

    case CHAR(c) => (Chr(c), s.substring(1), tr)

    case ALT(r1, r2) =>
      (tr: @unchecked) match {
        case 0 :: rest =>
          val (v, s1, tr1) = back_aux(r1, s, rest)
          (Left(v), s1, tr1)
        case 1 :: rest =>
          val (v, s1, tr1) = back_aux(r2, s, rest)
          (Right(v), s1, tr1)
      }

    case SEQ(r1, r2) =>
      val (v1, s1, tr1) = back_aux(r1, s, tr)
      val (v2, s2, tr2) = back_aux(r2, s1, tr1)
      (Sequ(v1, v2), s2, tr2)
    
    case STAR(r1) =>
      (tr: @unchecked) match {
        case 1 :: rest => (Stars(Nil), s, rest)
        case 0 :: rest =>
          val (v, s1, tr1) = back_aux(r1, s, rest)
          val (vs, s2, tr2) = back_aux(STAR(r1), s1, tr1)
          vs match {
            case Stars(vs1) => (Stars(v :: vs1), s2, tr2)
          }
      }
  }

def mat(r: Rexp, s: String): (Marks, Array[Trace]) = {
  val trace = new Array[Trace](s.length + 1)
  trace(0) = Nil
  shifts(List(0), trace, s, r)
}

def matcher(r: Rexp, s: String): Boolean = {
  val (_, trace) = mat(r, s)
  trace(s.length) != null
}

def lexer(r: Rexp, s: String, debug: Boolean): Val =
  s match {
    case "" => if (nullable(r)) mkeps(r) else Invalid

    case _ =>
      val (ms, trace) = mat(r, s)
      if (debug) {
        println(s"Rexp:\n${pp(r)}")
        println(s"s=$s")
        ppTraceArray(trace, s.length)
        println(s"Marks: $ms")
      }
      if (trace(s.length) == null) Invalid
      else back(r, s, trace(s.length))
  }



@main
def test1() =
  val reg = ((("a") ~ ("b"|ONE)) ~ ((ONE ~ "b") ~ %("b")))
  val s   = "abb"
  val marks=lexer(reg, s,true)
  val der=re_bitrev3.blexer_simp(reg, s)
  println(s"Marks Value=${marks}")
  //println(s"Marks Time= ${time_needed(100,lexer(reg, s,false))}")
  println(s"Derivative Value=${der}")
  //println(s"Derivative Time= ${time_needed(100,re_bitrev3.blexer_simp(reg, s))}")
  println(s"Equal Values: ${marks == der}")
  println("-" * 40)

@main
def test2() =
  val reg = %( %("a") | %("aa") | %("aaa") | %("aaaa") | %("aaaaa") ) 
  val s   = "a" * 1000
  val marks=lexer(reg, s,false)
  val der=re_bitrev3.blexer_simp(reg, s)
  println(s"Marks Value=${marks}")
  println(s"Marks Time= ${time_needed(1,lexer(reg, s,false))}")
  println(s"Derivative Value=${der}")
  println(s"Derivative Time= ${time_needed(1,re_bitrev3.blexer_simp(reg, s))}")
  println(s"Equal Values: ${marks == der}")
  println("-" * 40)


@main
def testall() = {
  given rexp_cdata: CDATA[Rexp] = List(
    (0, _ => ONE),
    (0, _ => ZERO),
    (0, _ => CHAR('a')),
    (0, _ => CHAR('b')),
    (0, _ => CHAR('c')),
    (1, cs => STAR(cs(0))),
    //(1, cs => NTIMES(cs(0), new scala.util.Random().nextInt(30) + 1)),
    (2, cs => ALT(cs(0), cs(1))),
    (2, cs => SEQ(cs(0), cs(1)))
  )

  val alphabet = LazyList('a', 'b')

  val numRegexes: Long = 10_000_000_000L
  val batchSize: Long  = 100_000L

  val parallelism = math.max(1, Runtime.getRuntime.availableProcessors() / 2)

  val batches = (0L to numRegexes by batchSize).par
  batches.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(parallelism))

  batches.foreach { start =>
    val end = math.min(start + batchSize - 1, numRegexes)
    var i = start
    while (i <= end) {
      val r = enumerate.decode(BigInt(i))
      if (i % 100_000L == 0L) { print("*") }

      val it = regenerate.generate_up_to(alphabet)(10)(r).iterator
      var k = 0
      while (k < 9 && it.hasNext) {
        var s = ""
        try {
            s = it.next()
            if (s != "") {
            val vm = lexer(r, s,false)
            val vb = re_bitrev3.blexer_simp(r, s)
            if (vm != vb) {
                println(s"$r and $s")
                println(s"vm=$vm vb=$vb")
                sys.exit(1)
            }
            }
            k += 1
        } catch {
          case e: Exception =>
            println(s"Error for regex ${e.getMessage} \n${pp(r)}")
            println(s"String '${s}':")
            val vb = re_bitrev3.blexer_simp(r, s)
            println(s"Derivative value = ${vb}") 
            println(s"Marks value      = ${lexer(r, s,false)}") 

            System.exit(1)
        }
      }

      i += 1
    }
  }
}

//test cases without Star
@main
def tests() =
  def run(reg: Rexp, s: String): (Int, Int, Double, Double) =
    val mar = lexer(reg, s, false)
    val der = re_bitrev3.blexer_simp(reg, s)
    val sameValue = der == mar
    println(s"\nMarks==Derivatives: $sameValue\n")
    val mtime = time_needed(1000000, lexer(reg, s, false))
    val dtime = time_needed(1000000, re_bitrev3.blexer_simp(reg, s))

    val timeResult =
      if mtime < dtime then "(Marks faster)"
      else if dtime < mtime then "(Derivatives faster)"
      else "(Equal time)"

    print(s"Derivative Time= $dtime")
    println(s" Marks Time= $mtime $timeResult")
    println(s"Marks Value= $mar")
    println(s"Derivative Value= $der")
    println("-" * 40)
    val marksFaster = if mtime < dtime then 1 else 0
    val equalValue = if sameValue then 1 else 0
    (marksFaster, equalValue, mtime, dtime)

  val cases: List[(Rexp, String)] = List(
    ((ONE | "a") ~ ("ab" | "b"), "ab"),
    ((ONE | "c") ~ (("c" ~ "c") | "c"), "cc"),
    (("aa") | ("a" ~ (ONE ~ "a")), "aa"),
    (((ONE ~ "a") | ("a" ~ ONE)), "a"),
    ((("a" | "b") | "b"), "b"),
    (("a" | ("ab" | "ba")), "ab"),
    ((("a" | "ab") ~ ("b" | ONE)), "ab"),
    ("abc", "abc"),
    ((("a" | ("a" ~ "a")) ~ ("a" | ("a" ~ "a"))), "aaa"),
    ((((("a" ~ "a") | "a") ~ ("a" | ("a" ~ "a")))), "aaa"),
    ((("a" | ("a" ~ "a")) ~ ("a" | ("a" ~ "a"))), "aaa"),
    (((("a" | "c") ~ ("c" ~ "b")) | (((ZERO ~ ONE) ~ ONE))), "acb"),
    (ONE | "a", "a"),
  )

  var marksFaster = 0
  var equalValue = 0
  var totalMarksTime = 0.0
  var totalDerTime = 0.0

  cases.zipWithIndex.foreach { case ((reg, s), idx) =>
    val i = idx + 1
    println(s"$i-")
    val (m, v, mt, dt) = run(reg, s)
    marksFaster += m
    equalValue += v
    totalMarksTime += mt
    totalDerTime += dt
  }

  val total = cases.length
  val avgMarksTime = totalMarksTime / total
  val avgDerTime = totalDerTime / total

  println(s"Marks were faster in $marksFaster test(s) out of $total.")
  println(s"Values matched in $equalValue test(s) out of $total.")
  println(s"Average Marks Time= $avgMarksTime")
  println(s"Average Derivative Time= $avgDerTime")


//test cases with Star
@main
def testsStar() =
  def run(reg: Rexp, s: String): (Int, Int, Double, Double) =
    val mar = lexer(reg, s, false)
    val der = re_bitrev3.blexer_simp(reg, s)

    val sameValue = der == mar

    println(s"\nMarks==Derivatives: $sameValue\n")

    val mtime = time_needed(100000, lexer(reg, s, false))
    val dtime = time_needed(100000, re_bitrev3.blexer_simp(reg, s))

    val timeResult =
      if mtime < dtime then "(Marks faster)"
      else if dtime < mtime then "(Derivatives faster)"
      else "(Equal time)"

    print(s"Derivative Time= $dtime")
    println(s" Marks Time= $mtime $timeResult")
    println(s"Marks Value= $mar")
    println(s"Derivative Value= $der")
    println("-" * 40)
    val marksFaster = if mtime < dtime then 1 else 0
    val equalValue = if sameValue then 1 else 0
    (marksFaster, equalValue, mtime, dtime)

  val cases: List[(Rexp, String)] = List(
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

  var marksFaster = 0
  var equalValue = 0
  var totalMarksTime = 0.0
  var totalDerTime = 0.0

  cases.zipWithIndex.foreach { case ((reg, s), idx) =>
    val i = idx + 1
    println(s"$i-")
    val (m, v, mt, dt) = run(reg, s)
    marksFaster += m
    equalValue += v
    totalMarksTime += mt
    totalDerTime += dt
  }

  val total = cases.length
  val avgMarksTime = totalMarksTime / total
  val avgDerTime = totalDerTime / total

  println(s"Marks were faster in $marksFaster test(s) out of $total.")
  println(s"Values matched in $equalValue test(s) out of $total.")
  println(s"Average Marks Time= $avgMarksTime")
  println(s"Average Derivative Time= $avgDerTime")

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

def ppTraceArray(trace: Array[Trace], n: Int): Unit ={
  println("===== Trace Array =====")
  for (i <- 0 to n) {
    val tr = trace(i)
    if (tr == null) println(s"$i: Empty")
    else println(s"$i: ${tr.mkString("[", ", ", "]")}")
  }
}
