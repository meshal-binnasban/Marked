import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.rexps, rexps._
import $file.rebit
import scala.collection.mutable
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._

type Record     = (Int, Int, Int, Int)   // (id, a, b, choice)
type StarRecord = (Int, Int, Int)        // (starId, b, prev)

case class Trace(t: List[Record], st: List[StarRecord])

type Marks2 = Array[Trace]               // length = s.length + 1, null means unreachable


def shifts2(ms: Marks2, s: String, r: RexpS): Marks2 =
  val n = s.length
  r match
    case ZEROS => new Array[Trace](n + 1)
    case ONES => new Array[Trace](n + 1)

    case CHARS(c) =>
      val out = new Array[Trace](n + 1)
      for (m <- 0 until n) {
        val tr = ms(m)
        if (tr != null && s(m) == c && out(m + 1) == null) out(m + 1) = tr
      }
      out

    case ALTS(r1, r2, id) =>
      val out = new Array[Trace](n + 1)

      for (start <- 0 to n) {
        val tr0 = ms(start)
        if (tr0 != null) {
          val ms0 = new Array[Trace](n + 1)
          ms0(start) = tr0

          val ms1 = shifts2(ms0, s, r1)
          for (b <- 0 to n) {
            val tr1 = ms1(b)
            if (tr1 != null && out(b) == null) {
              out(b) = Trace((id, start, b, 0) :: tr1.t, tr1.st)
            }
          }

          val ms2 = shifts2(ms0, s, r2)
          for (b <- 0 to n) {
            val tr2 = ms2(b)
            if (tr2 != null && out(b) == null) {
              out(b) = Trace((id, start, b, 1) :: tr2.t, tr2.st)
            }
          }
        }
      }

      out

    case SEQS(r1, r2, id) =>
      val out = new Array[Trace](n + 1)
      val n1  = nullableS(r1)
      val n2  = nullableS(r2)

      for (start <- 0 to n) {
        val tr0 = ms(start)
        if (tr0 != null) {
          val ms0 = new Array[Trace](n + 1)
          ms0(start) = tr0

          val ms1 = shifts2(ms0, s, r1)

          if (n2) {
            for (b <- n to 0 by -1) {
              val tr1 = ms1(b)
              if (tr1 != null && out(b) == null) {
                out(b) = Trace((id, start, b, b) :: tr1.t, tr1.st)
              }
            }
          }

          for (split <- n to 0 by -1) {
            val trSplit = ms1(split)
            if (trSplit != null) {
              val msS = new Array[Trace](n + 1)
              msS(split) = trSplit

              val ms2 = shifts2(msS, s, r2)
              for (b <- 0 to n) {
                val tr2 = ms2(b)
                if (tr2 != null && out(b) == null) {
                  out(b) = Trace((id, start, b, split) :: tr2.t, tr2.st)
                }
              }
            }
          }
2
          if (n1) {
            val ms2 = shifts2(ms0, s, r2)
            for (b <- 0 to n) {
              val tr2 = ms2(b)
              if (tr2 != null && out(b) == null) {
                out(b) = Trace((id, start, b, start) :: tr2.t, tr2.st)
              }
            }
          }
        }
      }
      out

    case STARSS(r1, id) =>
      def any(msx: Marks2): Boolean = (0 to n).exists(i => msx(i) != null)

      def unionFirst(a: Marks2, b: Marks2): Marks2 =
        val out = new Array[Trace](n + 1)
        for (i <- 0 to n) {
          val ta = a(i)
          out(i) = if (ta != null) ta else b(i)
        }
        out

      def go(msCur: Marks2, seen: Array[Boolean]): Marks2 =
        val msNext = new Array[Trace](n + 1)
        for (start <- n to 0 by -1) {
          val tr0 = msCur(start)
          if (tr0 != null && !seen(start)) {
            seen(start) = true

            val ms0 = new Array[Trace](n + 1)
            ms0(start) = tr0

            val ms1 = shifts2(ms0, s, r1)
            for (b <- n to 0 by -1) {
              val tr1 = ms1(b)
              if (tr1 != null && msNext(b) == null) {
                msNext(b) = Trace(tr1.t, (id, b, start) :: tr1.st)
              }
            }
          }
        }

        if (!any(msNext)) msNext
        else unionFirst(msNext, go(msNext, seen))

      val seen = new Array[Boolean](n + 1)
      go(ms, seen)

    case NTIMESS(r1, k) =>
      if (k == 0) Array.ofDim[Trace](n + 1)
      else if (k == 1) shifts2(ms, s, r1)
      else {
        val ms1 = shifts2(ms, s, r1)
        val ms2 = shifts2(ms1, s, NTIMESS(r1, k - 1))

        val out = Array.ofDim[Trace](n + 1)
        if (nullableS(r1)) {
          for (i <- 0 to n) {
            val t1 = ms1(i)
            out(i) = if (t1 != null) t1 else ms2(i)
          }
        } else {
          for (i <- 0 to n) out(i) = ms2(i)
        }
        out
      }

    case ANDS(r1, r2) =>
      val ms1 = shifts2(ms, s, r1)
      val ms2 = shifts2(ms, s, r2)
      val out = Array.ofDim[Trace](n + 1)
      for (i <- 0 to n) {
        val t1 = ms1(i)
        val t2 = ms2(i)
        if (t1 != null && t2 != null) out(i) = t1
      }
      out


def back2(r: RexpS, s: String, a: Int, b: Int, tr: Trace): Val =
  r match
    case ONES => if (a == b) Empty else Invalid

    case CHARS(c) => if (b == a + 1 && a >= 0 && b <= s.length && s(a) == c) Chr(c) else Invalid

    case ALTS(r1, r2, id) =>
      val ch = tr.t.reverseIterator.collectFirst { case (i, aa, bb, choice) if i == id && aa == a && bb == b => choice }
      ch match
        case Some(0) =>
          back2(r1, s, a, b, tr) match
            case Invalid => Invalid
            case v       => Left(v)
        case Some(1) =>
          back2(r2, s, a, b, tr) match
            case Invalid => Invalid
            case v       => Right(v)
        case _ =>
          Invalid

    case SEQS(r1, r2, id) =>
      val kopt = tr.t.reverseIterator.collectFirst { case (i, aa, bb, k) if i == id && aa == a && bb == b => k }
      kopt match
        case Some(k) if k == b =>
          back2(r1, s, a, b, tr) match
            case Invalid => Invalid
            case v1      => Sequ(v1, rexps.mkeps(r2))

        case Some(k) if k == a =>
          back2(r2, s, a, b, tr) match
            case Invalid => Invalid
            case v2      => Sequ(rexps.mkeps(r1), v2)

        case Some(k) =>
          (back2(r1, s, a, k, tr), back2(r2, s, k, b, tr)) match
            case (Invalid, _) => Invalid
            case (_, Invalid) => Invalid
            case (v1, v2)     => Sequ(v1, v2)

        case None =>
          Invalid

    case STARSS(r1, id) =>
      if (a == b) Stars(Nil)
      else
        val prevOpt = tr.st.reverseIterator.collectFirst { case (sid, bb, prev) if sid == id && bb == b => prev }
        prevOpt match
          case None => Invalid
          case Some(prev) =>
            (back2(STARSS(r1, id), s, a, prev, tr), back2(r1, s, prev, b, tr)) match
              case (Stars(vs), v) if v != Invalid => Stars(vs :+ v)
              case _                              => Invalid

    case _ =>
      Invalid

def mat2(r: RexpS, s: String): Marks2 =
  val n = s.length
  val ms0 = Array.ofDim[Trace](n + 1)
  ms0(0) = Trace(Nil, Nil)
  shifts2(ms0, s, r)

def matcher2(r: RexpS, s: String): Boolean =
  val n  = s.length
  val ms = mat2(r, s)
  ms(n) != null

def lexer2(r: Rexp, s: String): Val =
  val rs = intern(r)
  val n  = s.length
  val ms = mat2(rs, s)
  if (ms(n) == null) Invalid
  else back2(rs, s, 0, n, ms(n))


@main
def tests() =
  val reg = ("aa") | ("a" ~ (ONE ~ "a"))
  val s   = "aa"
  val der = rebit.blexer(reg, s)
  val mar = lexer2(reg, s)
  println(s"Derivative Value=${der}")
  println(s"Marks2 Value=${mar}")
  println(s"Der==Mar: ${der == mar}")
  val mtime= time_needed(100, lexer2(reg, s) )
  val dtime= time_needed(100, rebit.blexer(reg, s) )
  println(s"Marks2 time: $mtime")
  println(s"Derivative time: $dtime")
  println("-" * 40)

  val reg2 = %("a" | "aa")
  val s2   = "aaa"
  val der2 = rebit.blexer(reg2, s2)
  val mar2 = lexer2(reg2, s2)
  println(s"Derivative Value=${der2}")
  println(s"Marks2 Value=${mar2}")
  println(s"Der==Mar: ${der2 == mar2}")
  val mtime2= time_needed(100, lexer2(reg2, s2) )
  val dtime2= time_needed(100, rebit.blexer(reg2, s2 ) )
  println(s"Marks2 time: $mtime2")
  println(s"Derivative time: $dtime2")
  println("-" * 40)

  val reg3 = (ONE | %("a"))
  val s3   = "a"
  val der3 = rebit.blexer(reg3, s3)
  val mar3 = lexer2(reg3, s3)
  println(s"Derivative Value=${der3}")
  println(s"Marks2 Value=${mar3}")
  println(s"Der==Mar: ${der3 == mar3}")
  val mtime3= time_needed(100, lexer2(reg3, s3) )
  val dtime3= time_needed(100, rebit.blexer(reg3, s3 ) )
  println(s"Marks2 time: $mtime3")
  println(s"Derivative time: $dtime3")
  println("-" * 40)

  val reg4 = ("a" | %("a"))
  val s4   = "a"
  val der4 = rebit.blexer(reg4, s4)
  val mar4 = lexer2(reg4, s4)
  println(s"Derivative Value=${der4}")
  println(s"Marks2 Value=${mar4}")
  println(s"Der==Mar: ${der4 == mar4}")
  val mtime4= time_needed(100, lexer2(reg4, s4) )
  val dtime4= time_needed(100, rebit.blexer(reg4, s4 ) )
  println(s"Marks2 time: $mtime4")
  println(s"Derivative time: $dtime4")
  println("-" * 40)

  val reg5 = (%(ONE) ~ ("a"))
  val s5   = "a"
  val der5 = rebit.blexer(reg5, s5)
  val mar5 = lexer2(reg5, s5)
  println(s"Derivative Value=${der5}")
  println(s"Marks2 Value=${mar5}")
  println(s"Der==Mar: ${der5 == mar5}")
  val mtime5= time_needed(100, lexer2(reg5, s5) )
  val dtime5= time_needed(100, rebit.blexer(reg5, s5 ) )
  println(s"Marks2 time: $mtime5")
  println(s"Derivative time: $dtime5")
  println("-" * 40)

  val reg6 = (%("a") | %("aa"))
  val s6   = "aa"
  val der6 = rebit.blexer(reg6, s6)
  val mar6 = lexer2(reg6, s6)
  println(s"Derivative Value=${der6}")
  println(s"Marks2 Value=${mar6}")
  println(s"Der==Mar: ${der6 == mar6}")
  val mtime6= time_needed(100, lexer2(reg6, s6) )
  val dtime6= time_needed(100, rebit.blexer(reg6, s6 ) )
  println(s"Marks2 time: $mtime6")
  println(s"Derivative time: $dtime6")
  println("-" * 40)

  val reg7 = ((ONE | "a") ~ %("a"))
  val s7   = "a"
  val der7 = rebit.blexer(reg7, s7)
  val mar7 = lexer2(reg7, s7)
  println(s"Derivative Value=${der7}")
  println(s"Marks2 Value=${mar7}")
  println(s"Der==Mar: ${der7 == mar7}")
  val mtime7= time_needed(100, lexer2(reg7, s7) )
  val dtime7= time_needed(100, rebit.blexer(reg7, s7 ) )
  println(s"Marks2 time: $mtime7")
  println(s"Derivative time: $dtime7")
  println("-" * 40)

  val reg8 = (ONE ~ "a") | ("a" ~ ONE)
  val s8   = "a"
  val der8 = rebit.blexer(reg8, s8)
  val mar8 = lexer2(reg8, s8)
  println(s"Derivative Value=${der8}")
  println(s"Marks2 Value=${mar8}")
  println(s"Der==Mar: ${der8 == mar8}")
  val mtime8= time_needed(100, lexer2(reg8, s8) )
  val dtime8= time_needed(100, rebit.blexer(reg8, s8 ) )
  println(s"Marks2 time: $mtime8")
  println(s"Derivative time: $dtime8")
  println("-" * 40)

  val reg9 = (("a" ~ ONE) | (ONE ~ "a")) ~ %("a")
  val s9   = "aaaaaaaaa"
  val der9 = rebit.blexer(reg9, s9)
  val mar9 = lexer2(reg9, s9)
  println(s"Derivative Value=${der9}")
  println(s"Marks2 Value=${mar9}")
  println(s"Der==Mar: ${der9 == mar9}")
  val mtime9= time_needed(100, lexer2(reg9, s9) )
  val dtime9= time_needed(100, rebit.blexer(reg9, s9 ) )
  println(s"Marks2 time: $mtime9")
  println(s"Derivative time: $dtime9")
  println("-" * 40)

  val reg10 = (("a" | "b") | "b")
  val s10   = "b"
  val der10 = rebit.blexer(reg10, s10)
  val mar10 = lexer2(reg10, s10)
  println(s"Derivative Value=${der10}")
  println(s"Marks2 Value=${mar10}")
  println(s"Der==Mar: ${der10 == mar10}")
  val mtime10= time_needed(100, lexer2(reg10, s10) )
  val dtime10= time_needed(100, rebit.blexer(reg10, s10 ) )
  println(s"Marks2 time: $mtime10")
  println(s"Derivative time: $dtime10")
  println("-" * 40)

  val reg11 = ("a" | ("ab" | "ba"))
  val s11   = "ab"
  val der11 = rebit.blexer(reg11, s11)
  val mar11 = lexer2(reg11, s11)
  println(s"Derivative Value=${der11}")
  println(s"Marks2 Value=${mar11}")
  println(s"Der==Mar: ${der11 == mar11}")
  val mtime11= time_needed(100, lexer2(reg11, s11) )
  val dtime11= time_needed(100, rebit.blexer(reg11, s11 ) )
  println(s"Marks2 time: $mtime11")
  println(s"Derivative time: $dtime11")
  println("-" * 40)

  val reg12 = ("a" | "ab") ~ ("b" | ONE)
  val s12   = "ab"
  val der12 = rebit.blexer(reg12, s12)
  val mar12 = lexer2(reg12, s12)
  println(s"Derivative Value=${der12}")
  println(s"Marks2 Value=${mar12}")
  println(s"Der==Mar: ${der12 == mar12}")
  val mtime12= time_needed(100, lexer2(reg12, s12) )
  val dtime12= time_needed(100, rebit.blexer(reg12, s12 ) )
  println(s"Marks2 time: $mtime12")
  println(s"Derivative time: $dtime12")
  println("-" * 40)

@main
def test1() =
  val reg2 = %("a" | "aa")
  val s2   = "aaa"
  println(s"Derivative Value=${rebit.blexer(reg2, s2)}")
  println(lexer2(reg2, s2))
  println("-" * 40)

@main
def test2() =
  val reg13 = (%("a" | "b") )
  val s13   = "aba"
  println(s"13- $s13: r=\n${ppp(intern(reg13))} ")
  println(s"Derivative Value=${rebit.blexer(reg13, s13)}")
  println(s"Marks=  ${lexer2(reg13, s13)}")
  println("-" * 40) 
  val ms = mat2(intern(reg13), s13)
  val n  = s13.length
  for (i <- 0 to n) {
    val tr = ms(i)
    if (tr == null) {
        println(s"$i: null")
    } else {
        println(s"$i: Trace(t=${tr.t}, st=${tr.st})")
    }
    }

@main
def test3() =
  val reg12 = (ONE | "a") ~ ("ab" | "b")
    //(ONE | "c") ~ (("c" ~ "c") | "c")
    //("a"|ONE)~ %("a") 
    //%( %("a") | %("aa") | %("aaa") | %("aaaa") | %("aaaaa") ) 
  val s12   = "ab" 
  println(s"12- $s12: r=\n${ppp(intern(reg12))} ")

  val mar12 = lexer2(reg12, s12)
  println(s"Marks2 Value=${mar12}")
  val mtime12= time_needed(100, mat2(intern(reg12), s12) )
  println(s"Marks2 time: $mtime12")
  println(s"Derivative Value=${rebit.blexer(reg12, s12)}")

  println("-" * 40) 
  val ms = mat2(intern(reg12), s12)
  val n  = s12.length
  for (i <- 0 to n) {
    val tr = ms(i)
    if (tr == null) {
        println(s"$i: null")
    } else {
        println(s"$i: Trace(t=${tr.t}, st=${tr.st})")
    }
    }


@main
def testall() =
  given rexp_cdata: CDATA[Rexp] = List(
    (0, _ => ONE),
    (0, _ => ZERO),
    (0, _ => CHAR('a')),
    (0, _ => CHAR('b')),
    (0, _ => CHAR('c')),
   // (1, cs => STAR(cs(0))),
    (2, cs => ALT(cs(0), cs(1))),
    (2, cs => SEQ(cs(0), cs(1)))
  )

  val alphabet = LazyList('a', 'b')

  for (i <- 0L to 1_000_000_000L) {
    val r = decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- generate_up_to(alphabet)(20)(r).take(19) if s != "") {
      val vm  = lexer2(r, s)
      val vb  = rebit.blexer(r, s)
      val res = vm == vb
      if (!res) {
        println(s"$r and $s")
        println(s"vm=$vm vb=$vb")
        System.exit(1)
      }
    }
  }


def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}