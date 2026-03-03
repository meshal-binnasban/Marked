import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.re_bitrev3


sealed trait TraceItem
case class Alt(a: Int, b: Int, choice: Int) extends TraceItem
case class Seq(a: Int, b: Int, split: Int) extends TraceItem
case class Star(b: Int, prev: Int) extends TraceItem


case class Trace(t: List[TraceItem])

case class F(a: Int, m: Int, tr: Trace)

type Frontier = List[F]



def buildFrontier(mss: List[Int], outStart: Array[Int], outTrace: Array[Trace]): Frontier = mss match {
    case Nil => Nil
    case b :: bs => F(outStart(b), b, outTrace(b)) :: buildFrontier(bs, outStart, outTrace)
  }

def insertAsc(x: Int, xs: List[Int]): List[Int] = xs match {
    case Nil => x :: Nil
    case h :: t =>
      if (x < h) x :: xs
      else if (x == h) xs
      else h :: insertAsc(x, t)
  }

def shiftF(fs: Frontier, s: String, r: Rexp): Frontier = {
  val n = s.length

  r match {
    case ZERO =>
      Nil

    case ONE =>
      Nil

    case CHAR(c) =>
      val outStart = Array.fill(n + 1)(-1)
      val outTrace = new Array[Trace](n + 1)
      var mss: List[Int] = Nil

      for (f <- fs) {
        val m = f.m
        if (m < n && s(m) == c) {
          val b = m + 1
          if (outTrace(b) == null) {
            outStart(b) = f.a
            outTrace(b) = f.tr
            mss = insertAsc(b, mss)
          }
        }
      }

      buildFrontier(mss, outStart, outTrace)

    case ALT(r1, r2) =>
      val outStart = Array.fill(n + 1)(-1)
      val outTrace = new Array[Trace](n + 1)
      var mss: List[Int] = Nil

      val lefts = shiftF(fs, s, r1)
      for (f1 <- lefts) {
        val b = f1.m
        if (outTrace(b) == null) {
          outStart(b) = f1.a
          outTrace(b) = Trace(Alt(f1.a, b, 0) :: f1.tr.t)
          mss = insertAsc(b, mss)
        }
      }

      val rights = shiftF(fs, s, r2)
      for (f2 <- rights) {
        val b = f2.m
        if (outTrace(b) == null) {
          outStart(b) = f2.a
          outTrace(b) = Trace(Alt(f2.a, b, 1) :: f2.tr.t)
          mss = insertAsc(b, mss)
        }
      }

      buildFrontier(mss, outStart, outTrace)

    case SEQ(r1, r2) =>
      val outStart = Array.fill(n + 1)(-1)
      val outTrace = new Array[Trace](n + 1)
      var mss: List[Int] = Nil

      val n1 = nullable(r1)
      val n2 = nullable(r2)

      def claim(start: Int, b: Int, tr: Trace): Unit =
        if (outTrace(b) == null) {
          outStart(b) = start
          outTrace(b) = tr
          mss = insertAsc(b, mss)
        }

      val lefts = shiftF(fs, s, r1)

      if (n2) {
        for (f1 <- lefts) {
          claim(f1.a, f1.m, Trace(Seq(f1.a, f1.m, f1.m) :: f1.tr.t))
        }
      }

      for (f1 <- lefts.reverse) {
        val split = f1.m
        val rightIn = List(F(split, split, f1.tr))
        val rights = shiftF(rightIn, s, r2)

        for (f2 <- rights) {
          claim(f1.a, f2.m, Trace(Seq(f1.a, f2.m, split) :: f2.tr.t))
        }
      }

      if (n1) {
        for (f0 <- fs) {
          val start = f0.a
          val rightIn = List(F(start, start, f0.tr))
          val rights = shiftF(rightIn, s, r2)

          for (f2 <- rights) {
            claim(start, f2.m, Trace(Seq(start, f2.m, start) :: f2.tr.t))
          }
        }
      }

      buildFrontier(mss, outStart, outTrace)

    case STAR(r1) =>
      val seen = new Array[Boolean](n + 1)
      val outStart = Array.fill(n + 1)(-1)
      val outTrace = new Array[Trace](n + 1)
      var outMss: List[Int] = Nil

      def claimOut(f: F): Unit = {
        val b = f.m
        if (outTrace(b) == null) {
          outStart(b) = f.a
          outTrace(b) = f.tr
          outMss = insertAsc(b, outMss)
        }
      }

      def claimNext(f: F, nextStart: Array[Int], nextTrace: Array[Trace], nextMss: List[Int]): List[Int] = {
        val b = f.m
        if (nextTrace(b) == null) {
          nextStart(b) = f.a
          nextTrace(b) = f.tr
          insertAsc(b, nextMss)
        } else nextMss
      }

      def loop(work: Frontier): Unit =
        if (work.nonEmpty) {
          val nextStart = Array.fill(n + 1)(-1)
          val nextTrace = new Array[Trace](n + 1)
          var nextMss: List[Int] = Nil

          for (w <- work) {
            val start = w.m
            if (!seen(start) && w.tr != null) {
              seen(start) = true

              val iterIn = List(F(start, start, w.tr))
              val iters = shiftF(iterIn, s, r1)

              for (f1 <- iters.reverse) {
                val trb = Trace(Star(f1.m, start) :: f1.tr.t)
                val cand = F(w.a, f1.m, trb)
                claimOut(cand)
                nextMss = claimNext(cand, nextStart, nextTrace, nextMss)
              }
            }
          }

          val nextWork = buildFrontier(nextMss, nextStart, nextTrace)
          loop(nextWork.reverse)
        }

      loop(fs)

      buildFrontier(outMss, outStart, outTrace)

    case AND(_, _) =>
      throw new RuntimeException("shiftF: AND not implemented in this version")

    case NTIMES(_, _) =>
      throw new RuntimeException("shiftF: NTIMES not implemented in this version")
  }
}


def mat(r: Rexp, s: String): Frontier = shiftF(List(F(0, 0, Trace(Nil))), s, r)

def matcher(r: Rexp, s: String): Boolean = s match {
    case "" => nullable(r)
    case _ =>
      val fs = mat(r, s)
      fs.exists(_.m == s.length)
  }

def lexer(r: Rexp, s: String, debug: Boolean = false): Val = s match {
    case "" =>
      if (nullable(r)) mkeps(r) else Invalid

    case _ =>
      val fs = mat(r, s)

      if (debug) {
        println(s"Rexp:\n${pp(r)}")
        println(s"s=$s")
        println(s"frontier=$fs")
        println(" ")
        println(s"m=0: value= ${if (nullable(r)) mkeps(r) else Invalid}")
        for (f <- fs) {
          println(s"m=${f.m}: value= ${back(r, s, 0, f.m, f.tr)._1}")
        }
        println(" ")
      }

      if (fs.isEmpty) Invalid
      else {
        val best = fs.last
        back(r, s, 0, best.m, best.tr)._1
      }
  }

def back(r: Rexp, s: String, a: Int, b: Int, tr: Trace): (Val, Trace) = r match {
    case ZERO =>
      throw new RuntimeException(s"back: ZERO a=$a b=$b")

    case ONE =>
      if (a == b) (Empty, tr)
      else throw new RuntimeException(s"back: ONE a=$a b=$b")

    case CHAR(c) =>
      if (b == a + 1 && a >= 0 && b <= s.length && s(a) == c) (Chr(c), tr)
      else throw new RuntimeException(s"back: CHAR($c) a=$a b=$b")

    case ALT(r1, r2) =>
      tr.t match {
        case Alt(aa, bb, 0) :: rest if aa == a && bb == b =>
          val (v, tr1) = back(r1, s, a, b, Trace(rest))
          (Left(v), tr1)

        case Alt(aa, bb, 1) :: rest if aa == a && bb == b =>
          val (v, tr1) = back(r2, s, a, b, Trace(rest))
          (Right(v), tr1)

        case _ =>
          throw new RuntimeException(s"back: ALT a=$a b=$b")
      }

    case SEQ(r1, r2) =>
      tr.t match {
        case Seq(aa, bb, k) :: rest if aa == a && bb == b =>
          val tr0 = Trace(rest)

          if (k == b) {
            val (v1, tr1) = back(r1, s, a, b, tr0)
            (Sequ(v1, mkeps(r2)), tr1)
          } else if (k == a) {
            val (v2, tr2) = back(r2, s, a, b, tr0)
            (Sequ(mkeps(r1), v2), tr2)
          } else {
            val (v2, tr2) = back(r2, s, k, b, tr0)
            val (v1, tr1) = back(r1, s, a, k, tr2)
            (Sequ(v1, v2), tr1)
          }

        case _ =>
          throw new RuntimeException(s"back: SEQ a=$a b=$b")
      }

    case STAR(r1) =>
      def starBack(cur: Int, tr0: Trace, acc: List[Val]): (Val, Trace) =
        if (cur == a) (Stars(acc), tr0)
        else {
          tr0.t match {
            case Star(bb, prev) :: rest if bb == cur =>
              val (v, tr1) = back(r1, s, prev, cur, Trace(rest))
              starBack(prev, tr1, v :: acc)

            case _ =>
              throw new RuntimeException(s"back: STAR a=$a b=$cur")
          }
        }

      if (a == b) (Stars(Nil), tr)
      else starBack(b, tr, Nil)

    case AND(_, _) =>
      throw new RuntimeException("back: AND not implemented in this version")

    case NTIMES(_, _) =>
      throw new RuntimeException("back: NTIMES not implemented in this version")
  }




@main
def tests() = {
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
}

@main
def test1() ={
  val reg = %( %("a") | %("aa") | %("aaa") | %("aaaa") | %("aaaaa") ) 
  val s   = "a" * 1000
  //println(s"Derivative Value=${re_bitrev3.blexer_simp(reg, s)}")
  println(s"Derivative Time= ${time_needed(1,re_bitrev3.blexer_simp(reg, s))}")
  //println(s"Marks Value=${lexer(reg, s,false)}")
  println(s"Marks Time= ${time_needed(10,lexer(reg, s,false))}")
  println("-" * 40)
}

@main
def test2() = {
  val reg = ((("a" | "c") ~ ("c" ~ "b")) | (((ZERO ~ ONE) ~ ONE)))
  val s   = "acb"
  println(lexer(reg, s,true))
  println(s"${re_bitrev3.blexer_simp(reg, s)}= Derivative Value")
  println(s"Marks Time= ${time_needed(100000,lexer(reg, s,false))}")
  println(s"Derivative Time= ${time_needed(100000,re_bitrev3.blexer_simp(reg, s))}")
  println("-" * 40)
}

@main
def test3() ={
  val reg = %("a" | "b")
  val s   = "abab"
  println(lexer(reg, s,true))
  println(s"${re_bitrev3.blexer_simp(reg, s)}= Derivative Value")
  println(s"Marks Time= ${time_needed(100000,lexer(reg, s,false))}")
  println(s"Derivative Time= ${time_needed(100000,re_bitrev3.blexer_simp(reg, s))}")
  println("-" * 40)
}

@main
def test4() = {
  val reg = %("a" | "aa")
  val s   = "aaa"
  println(lexer(reg, s,true))
  println(s"${re_bitrev3.blexer_simp(reg, s)}= Derivative Value")
  println(s"Marks Time= ${time_needed(100000,lexer(reg, s,false))}")
  println(s"Derivative Time= ${time_needed(100000,re_bitrev3.blexer_simp(reg, s))}")
  println("-" * 40)
}

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}