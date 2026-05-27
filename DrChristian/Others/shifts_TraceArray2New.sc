import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.re_bitrev3

import scala.collection.parallel.ForkJoinTaskSupport
import java.util.concurrent.ForkJoinPool
import $ivy.`org.scala-lang.modules::scala-parallel-collections:1.0.4`
import scala.collection.parallel.CollectionConverters._

sealed trait TraceItem
case class Alt(choice: Int) extends TraceItem
case class Seq(split: Int) extends TraceItem
case class Star(prev: Int) extends TraceItem

case class Trace(t: List[TraceItem])

type Marks = List[Int]

def shift(ms: Marks, trace: Array[Trace], s: String, r: Rexp): (Marks, Array[Trace]) = {
  val n = s.length
  r match {
    case ZERO => (Nil, new Array[Trace](n + 1))
    case ONE => (Nil, new Array[Trace](n + 1))

    case CHAR(c) =>
      val outTrace = new Array[Trace](n + 1)
      var mss: List[Int] = Nil
      for (m <- ms) {
        if (m < n && s(m) == c) {
          val b = m + 1
          if (outTrace(b) == null) {
            outTrace(b) = trace(m)
            mss = b :: mss
          }
        }
      }
      (mss.reverse, outTrace)

    case ALT(r1, r2) =>
      val outTrace = new Array[Trace](n + 1)
      for (a <- ms) {
        val (ms1, tr1) = shift(List(a), trace, s, r1)
        for (b <- ms1) {
          if (outTrace(b) == null) outTrace(b) = Trace(Alt(0) :: tr1(b).t)
        }
        val (ms2, tr2) = shift(List(a), trace, s, r2)
        for (b <- ms2) {
          if (outTrace(b) == null) outTrace(b) = Trace(Alt(1) :: tr2(b).t)
        }
      }
      val mss = (0 to n).toList.filter(i => outTrace(i) != null)
      (mss, outTrace)

    case SEQ(r1, r2) =>
      val outTrace = new Array[Trace](n + 1)
      val n1 = nullable(r1)
      val n2 = nullable(r2)
      for (start <- ms) {
        val (ms1, tr1) = shift(List(start), trace, s, r1)

        if (n2) {
          for (b <- ms1) {
            if (outTrace(b) == null) outTrace(b) = Trace(Seq(b - start) :: tr1(b).t)
          }
        }

        for (split <- ms1.reverse) {
          val (ms2, tr2) = shift(List(split), tr1, s, r2)
          for (b <- ms2) {
            if (outTrace(b) == null) outTrace(b) = Trace(Seq(split - start) :: tr2(b).t)
          }
        }

        if (n1) {
          val (ms2, tr2) = shift(List(start), trace, s, r2)
          for (b <- ms2) {
            if (outTrace(b) == null) outTrace(b) = Trace(Seq(0) :: tr2(b).t)
          }
        }
      }
      val mss = (0 to n).toList.filter(i => outTrace(i) != null)
      (mss, outTrace)

    case STAR(r) =>
      def go(ms: List[Int], trace: Array[Trace], seen: Array[Boolean], res: Array[Trace]): Array[Trace] =
        if (ms.isEmpty) res
        else {
          val nextTrace = new Array[Trace](n + 1)
          for (start <- ms) {
            if (!seen(start) && trace(start) != null) {
              seen(start) = true
              val (ms1, tr1) = shift(List(start), trace, s, r)
              for (b <- ms1.reverse) {
                val trb = Trace(Star(b - start) :: tr1(b).t)
                if (res(b) == null) res(b) = trb
                if (nextTrace(b) == null) nextTrace(b) = trb
              }
            }
          }
          val mss = (0 to n).toList.filter(i => nextTrace(i) != null)
          go(mss.reverse, nextTrace, seen, res)
        }

      val seen = new Array[Boolean](n + 1)
      val res  = new Array[Trace](n + 1)
      val out = go(ms, trace, seen, res)
      val mss = (0 to n).toList.filter(i => out(i) != null)
      (mss, out)

    case AND(_, _) => (Nil, new Array[Trace](n + 1))
    case NTIMES(_, _) => (Nil, new Array[Trace](n + 1))
  }
}

def mat(r: Rexp, s: String): (Marks, Array[Trace]) = {
  val n = s.length
  val trace0 = new Array[Trace](n + 1)
  trace0(0) = Trace(Nil)
  shift(List(0), trace0, s, r)
}

def matcher(r: Rexp, s: String): Boolean =
  s match {
    case "" => nullable(r)
    case _ =>
      val (_, trace) = mat(r, s)
      trace(s.length) != null
  }

def back(r: Rexp, s: String, tr: Trace): Val =
  back_aux(r, s, tr) match {
    case (v, "", Trace(Nil)) => v
    case _ => throw new Exception("Not decodable")
  }

def back_aux(r: Rexp, s: String, tr: Trace): (Val, String, Trace) =
  ((r: @unchecked)) match {
    case CHAR(c) if s(0) == c => (Chr(c), s.substring(1), tr)

    case ALT(r1, r2) =>
      (tr.t: @unchecked) match {
        case Alt(0) :: rest =>
          val (v, s1, tr1) = back_aux(r1, s, Trace(rest))
          (Left(v), s1, tr1)
        case Alt(1) :: rest =>
          val (v, s1, tr1) = back_aux(r2, s, Trace(rest))
          (Right(v), s1, tr1)
      }

    case SEQ(r1, r2) =>
      (tr.t: @unchecked) match {
        case Seq(n) :: rest =>
          val tr0 = Trace(rest)
          if (n == s.length) {
            val (v1, _, tr1) = back_aux(r1, s, tr0)
            (Sequ(v1, mkeps(r2)), "", tr1)
          } else if (n == 0) {
            val (v2, _, tr2) = back_aux(r2, s, tr0)
            (Sequ(mkeps(r1), v2), "", tr2)
          } else {
            val (v2, _, tr2) = back_aux(r2, s.substring(n), tr0)
            val (v1, _, tr1) = back_aux(r1, s.substring(0, n), tr2)
            (Sequ(v1, v2), "", tr1)
          }
      }

    case STAR(r) =>
      if (s == "") (Stars(Nil), "", tr)
      else {
        (tr.t: @unchecked) match {
          case Star(n) :: rest =>
            val k = s.length - n
            val (v, _, tr1) = back_aux(r, s.substring(k), Trace(rest))
            val (v0, _, tr2) = back_aux(STAR(r), s.substring(0, k), tr1)
            v0 match {
              case Stars(vs) => (Stars(vs :+ v), "", tr2)
            }
        }
      }
  }

def lexer(r: Rexp, s: String, debug: Boolean = false): Val =
  s match {
    case "" =>
      if (nullable(r)) mkeps(r) else Invalid

    case _ =>
      val (ms, trace) = mat(r, s)

      if (debug) {
        println(s"Rexp:\n${pp(r)}")
        println(s"s=$s")
        println(s"marks=$ms")
        for (i <- 0 to s.length) {
          val tr = trace(i)
          if (tr == null) println(s"$i: Empty") else println(s"$i: $tr")
        }
        println(" ")
        println(s"m=0: value= ${if (nullable(r)) mkeps(r) else Invalid}")
        for (m <- ms) {
          println(s"m=$m: value= ${back(r, s.substring(0, m), trace(m))}")
        }
        println(" ")
      }

      if (ms.isEmpty) Invalid
      else {
        val b = ms.max
        back(r, s.substring(0, b), trace(b))
      }
  }


@main
def tests() =
  def run(reg: Rexp, s: String): Unit =
    val mar = lexer(reg, s, true)
    val der = re_bitrev3.blexer_simp(reg, s)
    println(s"\nMarks==Derivatives: ${der == mar}\n")
    println(s"Marks Value= $mar")
    println(s"Derivative Value= $der")
    println("-" * 40)

  val cases: List[(Rexp, String)] = List(
    ((ONE | "a") ~ ("ab" | "b"),"ab"),
    ((ONE | "c") ~ (("c" ~ "c") | "c"), "cc"),
    (("aa") | ("a" ~ (ONE ~ "a")), "aa"),
    (((ONE ~ "a") | ("a" ~ ONE)), "a"),
    ((("a" | "b") | "b"), "b"),
    (("a" | ("ab" | "ba")), "ab"),
    ((("a" | "ab") ~ ("b" | ONE)), "ab"),
    ((%(ONE) ~ "a"), "a"),
    (("a" | %("a")), "a"),
    ((ONE | %("a")), "a"),
    (%("a" | "aa"), "aaa"),
    ((%("a") | %("aa")), "aa"),
    (((ONE | "a") ~ %("a")), "a"),
    (((("a" ~ ONE) | (ONE ~ "a")) ~ %("a")), "aaaaaaaaa"),
    (%("a" | "aa"), "aaa"),
    ((%("a" | "b") ),"aba"),
    (("a"|ONE)~ %("a") , ""),
    ("abc","abc"),
    ((("a" | ONE) ~ "a") ~ %("a"),"aaa"),
    (("b"~ONE| %("a")) ~ %("a"|"b") , "aab"),
    (((("b" ~ ONE) | %("b")) ~ %("b" | "c")) , "bbc"),
    ((("a" | ("a" ~ "a")) ~ ("a" | ("a" ~ "a"))) , "aaa"),
    (((("a" ~ "a") | "a") ~ ("a" | ("a" ~ "a"))), "aaa"),
    ((("a" | ("a" ~ "a")) ~ ("a" | ("a" ~ "a"))), "aaa"),
    (((("a" | "c") ~ ("c" ~ "b")) | (((ZERO ~ ONE) ~ ONE))), "acb")

  )

  cases.zipWithIndex.foreach { case ((reg, s), idx) =>
  val i = idx + 1
  println(s"$i-")
  run(reg, s)
}

@main
def test1() =
  val reg = %( %("a") | %("aa") | %("aaa") | %("aaaa") | %("aaaaa") ) 
  val s   = "a" * 1000
  //println(s"Derivative Value=${re_bitrev3.blexer_simp(reg, s)}")
  println(s"Derivative Time= ${time_needed(1,re_bitrev3.blexer_simp(reg, s))}")
  //println(s"Marks Value=${lexer(reg, s,false)}")
  println(s"Marks Time= ${time_needed(1,lexer(reg, s,false))}")
  println("-" * 40)

@main
def test2() =
  val reg = ((("b" ~ ONE) | %("b")) ~ %("b" | "c"))
  val s   = "bbc"
  println(s"Derivative Value=${re_bitrev3.blexer_simp(reg, s)}")
  println(s"Marks Value=${lexer(reg, s,true)}")
  println(s"Marks Time= ${time_needed(100,lexer(reg, s,false))}")
  println(s"Derivative Time= ${time_needed(100,re_bitrev3.blexer_simp(reg, s))}")
  println("-" * 40)

@main
def test3() =
  val reg = %("a" | "b")
  val s   = "abab"
  println(lexer(reg, s,true))
  println(s"${re_bitrev3.blexer_simp(reg, s)}= Derivative Value")
  println(s"Marks Time= ${time_needed(100000,lexer(reg, s,false))}")
  println(s"Derivative Time= ${time_needed(100000,re_bitrev3.blexer_simp(reg, s))}")
  println("-" * 40)

@main
def test4() =
  val reg = %("a" | "aa")
  val s   = "aaa"
  println(lexer(reg, s,true))
  println(s"${re_bitrev3.blexer_simp(reg, s)}= Derivative Value")
  println(s"Marks Time= ${time_needed(100000,lexer(reg, s,false))}")
  println(s"Derivative Time= ${time_needed(100000,re_bitrev3.blexer_simp(reg, s))}")
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
        val s = it.next()
        if (s != "") {
          val vm = lexer(r, s)
          val vb = re_bitrev3.blexer_simp(r, s)
          if (vm != vb) {
            println(s"$r and $s")
            println(s"vm=$vm vb=$vb")
            sys.exit(1)
          }
        }
        k += 1
      }

      i += 1
    }
  }
}

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}