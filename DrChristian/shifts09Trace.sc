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
//case class Star(prev: Int) extends TraceItem
case class Star(t1: Trace, t2: Trace) extends TraceItem
case object Eps extends TraceItem
case class Seq(t1: Trace, t2: Trace) extends TraceItem

type Trace = List[TraceItem]
//case class Trace(t: List[TraceItem])

type Marks = List[Int]

def shifts(ms: Marks, trace: Array[Trace], s: String, r: Rexp): (Marks, Array[Trace]) = {
  val n = s.length
  r match {
    case ZERO => (Nil, new Array[Trace](n + 1))
    case ONE => 
        val outTrace = new Array[Trace](n + 1)
        var mss: List[Int] = Nil
        for (m <- ms) {
            outTrace(m) = Eps :: trace(m)
            mss = m :: mss
        }
        (mss.reverse, outTrace) 

    case CHAR(c) =>  
        val outTrace = new Array[Trace](n + 1)
        var mss: List[Int] = Nil
        for (m <- ms) {
            if (m < n && s(m) == c) {
            outTrace(m+1) = trace(m)
            mss= (m+1) :: mss
            }
        }
        (mss.reverse, outTrace) 

    case ALT(r1, r2) =>
      val outTrace = new Array[Trace](n + 1)
      val (ms1, tr1) = shifts(ms, trace, s, r1)
      val (ms2, tr2) = shifts(ms, trace, s, r2)

      //record left first
      for (b <- ms1) outTrace(b) = Alt(0) :: tr1(b)

      //record right if reached further than left
      for (b <- ms2) {
        if (outTrace(b) == null) {
          outTrace(b) = Alt(1) :: tr2(b)
        }
      }
      val mss = (ms1 ::: ms2).distinct.sorted
      (mss, outTrace)

    case SEQ(r1, r2) =>
      val outTrace = new Array[Trace](n + 1)
      var mss: List[Int] = Nil 

      val (ms1, tr1) = shifts(ms, trace, s, r1)
      //reverse ms1 to shift further marks first.
      for (m <- ms1.reverse) {
        //fresh trace for r2 to record both traces of r1 (tr1) and r2 (tr2)
        val traceR2 = new Array[Trace](n + 1)
        traceR2(m) = Nil
        val (ms2, tr2) = shifts(List(m), traceR2, s, r2)
        
        for (b <- ms2) {
          if (outTrace(b) == null) {
            outTrace(b) = List(Seq(tr1(m), tr2(b)))
            mss = b :: mss 
          }
        }
      }
      (mss.sorted, outTrace)
    
    case STAR(r) =>
      // final trace for this STAR
      val outTrace = new Array[Trace](n + 1)

      // record the current recieved trace for zero repetition
      for (m <- ms) {
        outTrace(m) = trace(m)
      }

      // trace array for the next STAR iteration
      val nextTrace = new Array[Trace](n + 1)
      var ms0: List[Int] = Nil
      // fresh trace for one start mark
      for (start <- ms.reverse) {
        val traceR = new Array[Trace](n + 1)
        traceR(start) = Nil
        val (ms1, tr1) = shifts(List(start), traceR, s, r)
        // check the difference between produced marks and original ms
        for (b <- ms1.reverse) {
          if (!ms.contains(b) && nextTrace(b) == null) {
            // Star(one iteration trace, star trace already built up to start)
            nextTrace(b) = List(Star(tr1(b), trace(start)))
            ms0 = b :: ms0
          }
        }
      }

      //return the current trace and marks if no new marks are produced
      if (ms0.isEmpty) {
        (ms, outTrace)
      } else {
        //shift into the star if new marks are produced
        val (msRec, trRec) = shifts(ms0.reverse, nextTrace, s, STAR(r))
        for (b <- msRec) {
          if (outTrace(b) == null) {
            outTrace(b) = trRec(b)
          }
        }
        ((ms ::: msRec).distinct.sorted, outTrace)
      }
    case AND(_, _) =>(Nil, new Array[Trace](n + 1)) // not done yet
    case NTIMES(_, _) =>(Nil, new Array[Trace](n + 1))// not done yet
  }
}  

def mat(r: Rexp, s: String): (Marks, Array[Trace]) = {
  val n = s.length
  val trace0 = new Array[Trace](n + 1)
  trace0(0) = Nil
  shifts(List(0), trace0, s, r)
}



def matcher(r: Rexp, s: String): Boolean =
  s match {
    case "" => nullable(r)
    case _ =>
      val (_, trace) = mat(r, s)
      trace(s.length) != null
  } 


def lexer(r: Rexp, s: String, debug: Boolean = false): Val =
  s match {
    case "" => if (nullable(r)) mkeps(r) else Invalid

    case _ =>
      val (ms,trace) = mat(r, s)

      if (debug) {
        println(s"Rexp:\n${pp(r)}")
        println(s"s=$s")
        for (i <- 0 to s.length) {
          val tr = trace(i)
          if (tr == null) println(s"$i: Empty") else println(s"$i: $tr")
        }
        println(s"Marks: $ms")
      }

      if (trace(s.length) == null) Invalid
      else back(r, s, trace(s.length))
  }



def back(r: Rexp, s: String, tr: Trace): Val =
  back_aux(r, s, tr) match {
    case (v, "", Nil) => v
    case _ => throw new Exception("Not decodable")
  }

def back_aux(r: Rexp, s: String, tr: Trace): (Val, String, Trace) =
  (r: @unchecked) match {
    case ONE =>
      (tr: @unchecked) match {
        case Eps :: rest => (Empty, s, rest)
      }  
    case CHAR(c) => (Chr(c), s.substring(1), tr)
    
    case ALT(r1, r2) =>
      (tr: @unchecked) match {
        case Alt(0) :: rest =>
          val (v, s1, tr1) = back_aux(r1, s, rest)
          (Left(v), s1, tr1)
        case Alt(1) :: rest =>
          val (v, s1, tr1) = back_aux(r2, s, rest)
          (Right(v), s1, tr1)
      } 
    case SEQ(r1, r2) => 
        (tr: @unchecked) match {
            case Seq(tr1, tr2) :: rest =>
            val (v1, s1, t1) = (back_aux(r1, s, tr1))
            val (v2, s2, t2) = (back_aux(r2, s1, tr2))
            (Sequ(v1, v2), s2, rest)
        }
    case STAR(r) =>
       (tr: @unchecked) match {
        case Nil => (Stars(Nil), s, Nil)
        case Star(tr1, tr2) :: rest =>
          val (v, s1, t1) = back_aux(r, s, tr1)
          val (v0, s2, t2) = back_aux(STAR(r), s1, tr2)
          v0 match {
            case Stars(vs) => (Stars(v :: vs), s2, rest)
          }
      }
      
  }



@main
def test1() =
  val reg = %("a" | "aa")
  val s   = "aaa"
  println(s"Marks Value=${lexer(reg, s,true)}")
  println(s"Marks Time= ${time_needed(100,lexer(reg, s,false))}")
  println(s"Derivative Value=${re_bitrev3.blexer_simp(reg, s)}")
  println(s"Derivative Time= ${time_needed(100,re_bitrev3.blexer_simp(reg, s))}")
  println("-" * 40)

@main
def test2() =
  val reg = %("b" | %("a"))
    //%( ONE | (ONE | "a" ) ) | %("a" | %("b") ) 
  val s   = "bab"
  println(s"Marks Value=${lexer(reg, s,true)}")
  println(s"Marks Time= ${time_needed(5,lexer(reg, s,false))}")
  println(s"Derivative Value=${re_bitrev3.blexer_simp(reg, s)}")
  println(s"Derivative Time= ${time_needed(5,re_bitrev3.blexer_simp(reg, s))}")
  println("-" * 40)

@main
def test3() =
  val reg = %( %("a") | %("aa") | %("aaa") | %("aaaa") | %("aaaaa") ) 
  val s   = "a" * 1000
  println(s"Marks Value=${lexer(reg, s,true)}")
  println(s"Marks Time= ${time_needed(5,lexer(reg, s,false))}")
  println(s"Derivative Value=${re_bitrev3.blexer_simp(reg, s)}")
  println(s"Derivative Time= ${time_needed(5,re_bitrev3.blexer_simp(reg, s))}")
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
            val vm = lexer(r, s)
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
            println(s"Marks value      = ${lexer(r, s)}") 

            System.exit(1)
        }
      }

      i += 1
    }
  }
}

@main
def tests() =
  def run(reg: Rexp, s: String): (Int, Int) =
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
    (marksFaster, equalValue)

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
    (ONE| "a" , "a"),
  )

  var marksFaster = 0
  var equalValue = 0

  cases.zipWithIndex.foreach { case ((reg, s), idx) =>
    val i = idx + 1
    println(s"$i-")
    val (m, v) = run(reg, s)
    marksFaster += m
    equalValue += v
  }

  val total = cases.length
  println(s"Marks were faster in $marksFaster test(s) out of $total.")
  println(s"Values matched in $equalValue test(s) out of $total.")

@main
def testsStar() =
  def run(reg: Rexp, s: String): (Int, Int) =
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
    (marksFaster, equalValue)

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
    (("b" ~ ONE | %("a")) ~ %("a" | "b"), "aab")
  )

  var marksFaster = 0
  var equalValue = 0

  cases.zipWithIndex.foreach { case ((reg, s), idx) =>
    val i = idx + 1
    println(s"$i-")
    val (m, v) = run(reg, s)
    marksFaster += m
    equalValue += v
  }

  val total = cases.length
  println(s"Marks were faster in $marksFaster test(s) out of $total.")
  println(s"Values matched in $equalValue test(s) out of $total.")


def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}


