import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.rexps, rexps._
import $file.re_bitrev3
import scala.collection.mutable
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._

type Marks = List[Int]

type Record = (Int, Int, Int, Int)    // (id, a, b, choiceOrSplit)
type StarRecord = (Int, Int, Int)     // (starId, end b, prev)

case class Trace(t: List[Record], st: List[StarRecord])

def shift(ms: Marks, trace: Array[Trace], s: String, r: RexpS): (Marks, Array[Trace]) = {
  val n = s.length
  r match {
    case ZEROS => (Nil, new Array[Trace](n + 1))
    case ONES => (Nil, new Array[Trace](n + 1))
    
    case CHARS(c) =>
      val outTrace = new Array[Trace](n + 1)
      var mss: List[Int] = Nil

      for (m <- ms) {
        if (m < n && s(m) == c) {
          val b = m + 1
          outTrace(b) = trace(m)
          mss =  b :: mss
        }
      }
      (mss.reverse, outTrace)

    case ALTS(r1, r2, id) =>
      val outTrace = new Array[Trace](n + 1)
      for (a <- ms) {
        val (ms1, tr1) = shift(List(a), trace, s, r1)
        for (b <- ms1) {
          if (outTrace(b) == null) {
            val trb = tr1(b)
            outTrace(b) = Trace((id, a, b, 0) :: trb.t, trb.st)
          }
        }
        val (ms2, tr2) = shift(List(a), trace, s, r2)
        for (b <- ms2) {
          if (outTrace(b) == null) {
            val trb = tr2(b)
            outTrace(b) = Trace((id, a, b, 1) :: trb.t, trb.st)  
          }
        }
      }
      val mss = (0 to n).toList.filter(i => outTrace(i) != null)
      (mss, outTrace)

    case SEQS(r1, r2, id) =>
        val outTrace = new Array[Trace](n + 1)

        val n1 = nullableS(r1)
        val n2 = nullableS(r2)
        
        for (start <- ms) {
            val (ms1, tr1) = shift(List(start), trace, s, r1)
            if (n2) {
            for (b <- ms1) {
                if (outTrace(b) == null) {
                val trb = tr1(b)
                outTrace(b) = Trace((id, start, b, b) :: trb.t, trb.st)
                }
            }
            }
            for (split <- ms1.reverse) {
            val (ms2, tr2) = shift(List(split), tr1, s, r2)
            for (b <- ms2) {
                if (outTrace(b) == null) {
                val trb = tr2(b)
                outTrace(b) = Trace((id, start, b, split) :: trb.t, trb.st)
                }
            }
            }

            if (n1) {
            val (ms2, tr2) = shift(List(start), trace, s, r2)
            for (b <- ms2) {
                if (outTrace(b) == null) {
                val trb = tr2(b)
                outTrace(b) = Trace((id, start, b, start) :: trb.t, trb.st)
                }
            }
            }
        }
        val mss = (0 to n).toList.filter(i => outTrace(i) != null)
        (mss, outTrace)
    case STARSS(r1, id) =>

        def go(ms: List[Int], trace: Array[Trace], seen: Array[Boolean], res: Array[Trace]): Array[Trace] = {
            if (ms.isEmpty) res
            else {
            val nextTrace = new Array[Trace](n + 1)
            for (start <- ms.reverse) {
                if (!seen(start) && trace(start) != null) {
                seen(start) = true

                val (ms1, tr1) = shift(List(start), trace, s, r1)

                for (b <- ms1) {
                   val trb = Trace(tr1(b).t, (id, b, start) :: tr1(b).st)
                    if (res(b) == null)      res(b) = trb
                    if (nextTrace(b) == null) nextTrace(b) = trb
                }
                }
            }
            val mss = (0 to n).toList.filter(i => nextTrace(i) != null)
            go(mss, nextTrace, seen, res)
            }
        }
        val seen = new Array[Boolean](n + 1)
        val res  = new Array[Trace](n + 1)

        val out = go(ms, trace, seen, res)
        val mss = (0 to n).toList.filter(i => out(i) != null)

        (mss, out)


    case ANDS(r1, r2) =>
      val (ms1, tr1) = shift(ms, trace, s, r1)
      val (ms2, tr2) = shift(ms, trace, s, r2)

      val outTrace = new Array[Trace](n + 1)
      var ms3: List[Int] = Nil

      val s2 = ms2.toSet
      for (b <- ms1) {
        if (s2.contains(b)) {
          outTrace(b) = tr1(b)
          ms3 = b :: ms3
        }
      }
      (ms3.reverse, outTrace)
    case NTIMESS(_, _) => (Nil, new Array[Trace](n + 1))
  }
}

def mat(r: RexpS, s: String): (List[Int], Array[Trace]) = {
  val n = s.length
  val trace0 = new Array[Trace](n + 1)
  trace0(0) = Trace(Nil, Nil)
  shift(List(0), trace0, s, r)
}

def matcher(r: RexpS, s: String): Boolean = {
  s match{ 
    case "" => nullableS(r)
    case _ => 
        val (_, trace) = mat(r, s)
        trace(s.length) != null
  } 
}

def lexer(r: Rexp, s: String, debug: Boolean = false): Val = {
    s match{ 
        case "" => if (nullable(r)) rexp.mkeps(r) else Invalid
        case _ => 
            val rs = intern(r)
            val (ms, trace) = mat(rs, s)

            if (debug) {
                println(s"RexpS:\n${ppp(rs)}")
                println(s"s=$s")
                println(s"marks=$ms\n")
                for (i <- 0 to s.length) {
                val tr = trace(i)
                if (tr == null) println(s"$i: Empty") else println(s"$i: $tr")
                }
            }

            if (ms.isEmpty) Invalid
            else {
                if(debug){
                    println(" ")
                    println(s"m=0: value= ${if(nullableS(rs)) rexps.mkeps(rs) else Invalid}")
                    for(m <- ms){
                        back(rs,s,0,m,trace(m)) match {
                            case Invalid => println(s"Invalid at $m")
                            case v => println(s"m=$m: value= $v")
                        }
                    }
                    println(" ") 
                }
                val b = ms.max
                val tr = trace(b)
                if (tr == null) Invalid else back(rs, s, 0, b, tr)
            }
    }
}

def back(r: RexpS, s: String, a: Int, b: Int, tr: Trace): Val =
  r match {
    case ZEROS =>Invalid
    case ONES => if (a == b) Empty else Invalid
    case CHARS(c) => if (b == a + 1 && a >= 0 && b <= s.length && s(a) == c) Chr(c) else Invalid

    case ALTS(r1, r2, id) =>
      tr.t.find { case (rid, ra, rb, _) => rid == id && ra == a && rb == b } match {
        case Some((_, _, _, 0)) =>
          back(r1, s, a, b, tr) match {
            case Invalid => Invalid
            case v       => Left(v)
          }
        case Some((_, _, _, 1)) =>
          back(r2, s, a, b, tr) match {
            case Invalid => Invalid
            case v       => Right(v)
          }
        case _ =>
          Invalid
      }

    case SEQS(r1, r2, id) =>
      tr.t.find { case (rid, ra, rb, _) => rid == id && ra == a && rb == b } match {
        case Some((_, _, _, k)) if k == b =>
          back(r1, s, a, b, tr) match {
            case Invalid => Invalid
            case v1      => Sequ(v1, rexps.mkeps(r2))
          }

        case Some((_, _, _, k)) if k == a =>
          back(r2, s, a, b, tr) match {
            case Invalid => Invalid
            case v2      => Sequ(rexps.mkeps(r1), v2)
          }

        case Some((_, _, _, k)) =>
          (back(r1, s, a, k, tr), back(r2, s, k, b, tr)) match {
            case (Invalid, _) => Invalid
            case (_, Invalid) => Invalid
            case (v1, v2)     => Sequ(v1, v2)
          }

        case _ => Invalid
      }

    case STARSS(r1, id) =>
      if (a == b) Stars(Nil)
      else {
        tr.st.find { case (sid, end, _) => sid == id && end == b } match {
          case None => Invalid
          case Some((_, _, prev)) =>
            (back(STARSS(r1, id), s, a, prev, tr), back(r1, s, prev, b, tr)) match {
              case (Stars(vs), v) if v != Invalid => Stars(vs :+ v)
              case _                              => Invalid
            }
        }
      }

    case ANDS(r1, r2) =>
      val v1 = back(r1, s, a, b, tr)
      if (v1 == Invalid) Invalid
      else {
        val v2 = back(r2, s, a, b, tr)
        if (v2 == Invalid) Invalid else v1
      }

    case NTIMESS(_, _) => Invalid
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
  val s   = "a" * 3000
  println(s"Derivative Value=${re_bitrev3.blexer_simp(reg, s)}")
  println(s"Marks Value=${lexer(reg, s,false)}")
  println(s"Derivative Time= ${time_needed(100,re_bitrev3.blexer_simp(reg, s))}")
  println(s"Marks Time= ${time_needed(100,lexer(reg, s,false))}")
  println("-" * 40)

@main
def test2() =
  val reg = ((("a" | "c") ~ ("c" ~ "b")) | (((ZERO ~ ONE) ~ ONE)))
  val s   = "acb"
  println(lexer(reg, s,true))
  println(s"${re_bitrev3.blexer_simp(reg, s)}= Derivative Value")
  println(s"Marks Time= ${time_needed(100000,lexer(reg, s,false))}")
  println(s"Derivative Time= ${time_needed(100000,re_bitrev3.blexer_simp(reg, s))}")
  println("-" * 40)


@main
def testall() ={
  given rexp_cdata: CDATA[Rexp] = List(
    (0, _ => ONE),
    (0, _ => ZERO),
    (0, _ => CHAR('a')),
    (0, _ => CHAR('b')),
    (0, _ => CHAR('c')),
    (1, cs => STAR(cs(0))),
    (2, cs => ALT(cs(0), cs(1))),
    (2, cs => SEQ(cs(0), cs(1)))
  )

  val alphabet = LazyList('a', 'b')

  for (i <- 0L to 1_000_000_000L) {
    val r = decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- generate_up_to(alphabet)(20)(r).take(19) if s != "") {
      val vm  = lexer(r, s)
      val vb  = re_bitrev3.blexer_simp(r, s)
       val res = vm == vb
      if (!res) {
        println(s"$r and $s")
        println(s"vm=$vm vb=$vb")
        System.exit(1)
      } 
    }
  }
}


import $ivy.`org.scala-lang.modules::scala-parallel-collections:1.0.4`
import scala.collection.parallel.CollectionConverters._

@main
def testallp() = {
  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE),
        (0, _ => ZERO),
        (0, _ => CHAR('a')),
        (0, _ => CHAR('b')),
        (0, _ => CHAR('c')),
        (1, cs => STAR(cs(0))),
        //(1, cs => NTIMES(cs(0),new scala.util.Random().nextInt(30) + 1 )),
        (2, cs => ALT(cs(0), cs(1))),
        (2, cs => SEQ(cs(0), cs(1)))
      )
  val alphabet = LazyList('a', 'b')

  val numRegexes = BigInt(10_000_000_000L)
  val batchSize = BigInt(100_000L) 
  
  val batches = (BigInt(0) to numRegexes by batchSize).toVector.par
  batches.foreach { start =>
    val end = (start + batchSize - 1).min(numRegexes)
    for (i <- start to end) {
      val r = enumerate.decode(i)
      if (i % 100_000 == 0) { print("*") }
      for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "") {
        val vm  = lexer(r, s)
        val vb  = re_bitrev3.blexer_simp(r, s)
        val res = vm == vb
        if (!res) {
            println(s"$r and $s")
            println(s"vm=$vm vb=$vb")
            System.exit(1)
        } 

      }// end of for s <- regenerate.generate_up_to(alphabet)(10)(r)
    }//end of for i <- start to end
  }// end of batches.foreach
}// end of testAllP

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

import scala.collection.parallel.ForkJoinTaskSupport
import java.util.concurrent.ForkJoinPool
@main
def testallpp() = {
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
