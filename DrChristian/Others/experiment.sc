import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.rexps, rexps._
import $file.rebit
import scala.collection.mutable
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._

type Record     = (Int, Int, Int, Int)   // (id, a, b, choiceOrSplit)
type StarRecord = (Int, Int, Int)        // (starId, b, prev)

case class Trace(t: List[Record], st: List[StarRecord], src: Int, root: Int, mid: Int)

type Marks2 = Array[Trace]               // length = n+1, null means unreachable

def shifts2(ms: Marks2, s: String, r: RexpS): Marks2 =
  val n = s.length

  r match
    case ZEROS =>
      Array.ofDim[Trace](n + 1)

    case ONES =>
      Array.ofDim[Trace](n + 1)

    case CHARS(c) =>
      val out = Array.ofDim[Trace](n + 1)
      for (m <- 0 until n) {
        val tr = ms(m)
        if (tr != null && s(m) == c && out(m + 1) == null) out(m + 1) = tr
      }
      out

    case ALTS(r1, r2, id) =>
      val msIn = Array.ofDim[Trace](n + 1)
      for (i <- 0 to n) {
        val tr = ms(i)
        if (tr != null) msIn(i) = Trace(tr.t, tr.st, i, tr.root, tr.mid)
      }

      val out = Array.ofDim[Trace](n + 1)

      val ms1 = shifts2(msIn, s, r1)
      for (b <- 0 to n) {
        val tr1 = ms1(b)
        if (tr1 != null && out(b) == null) {
          val a = tr1.src
          out(b) = Trace((id, a, b, 0) :: tr1.t, tr1.st, tr1.src, tr1.root, tr1.mid)
        }
      }

      val ms2 = shifts2(msIn, s, r2)
      for (b <- 0 to n) {
        val tr2 = ms2(b)
        if (tr2 != null && out(b) == null) {
          val a = tr2.src
          out(b) = Trace((id, a, b, 1) :: tr2.t, tr2.st, tr2.src, tr2.root, tr2.mid)
        }
      }

      out

    case SEQS(r1, r2, id) =>
      val msIn = Array.ofDim[Trace](n + 1)
      for (i <- 0 to n) {
        val tr = ms(i)
        if (tr != null) msIn(i) = Trace(tr.t, tr.st, i, tr.root, tr.mid)
      }

      val ms1 = shifts2(msIn, s, r1)

      val ms12 = Array.ofDim[Trace](n + 1)
      for (i <- 0 to n) {
        val t1 = ms1(i)
        ms12(i) = if (t1 != null) t1 else if (nullableS(r1)) msIn(i) else null
      }

      val msR2 = Array.ofDim[Trace](n + 1)
      for (k <- 0 to n) {
        val tr = ms12(k)
        if (tr != null) {
          val root = tr.src
          msR2(k) = Trace(tr.t, tr.st, k, root, k)
        }
      }

      val ms2raw = shifts2(msR2, s, r2)

      val ms2 = Array.ofDim[Trace](n + 1)
      for (b <- 0 to n) {
        val tr = ms2raw(b)
        if (tr != null) {
          ms2(b) = Trace((id, tr.root, b, tr.mid) :: tr.t, tr.st, tr.src, tr.root, tr.mid)
        }
      }

      val out = Array.ofDim[Trace](n + 1)

      (nullableS(r1), nullableS(r2)) match
        case (true, true) =>
          val left = ms2
          for (i <- 0 to n) out(i) = left(i)
          for (i <- 0 to n) {
            val t1 = ms1(i)
            if (out(i) == null && t1 != null) out(i) = t1
          }
          out

        case (true, false) =>
          for (i <- 0 to n) out(i) = ms2(i)
          out

        case (false, true) =>
          val left = ms2
          for (i <- 0 to n) out(i) = left(i)
          for (i <- 0 to n) {
            val t1 = ms1(i)
            if (out(i) == null && t1 != null) out(i) = t1
          }
          out

        case (false, false) =>
          for (i <- 0 to n) out(i) = ms2(i)
          out

    case STARSS(r1, id) =>
      val msIn = Array.ofDim[Trace](n + 1)
      for (i <- 0 to n) {
        val tr = ms(i)
        if (tr != null) msIn(i) = Trace(tr.t, tr.st, i, tr.root, tr.mid)
      }

      val ms1raw = shifts2(msIn, s, r1)

      var any = false
      for (i <- 0 to n) if (!any && ms1raw(i) != null) any = true
      if (!any) Array.ofDim[Trace](n + 1)
      else {
        val ms1 = Array.ofDim[Trace](n + 1)
        for (b <- 0 to n) {
          val tr = ms1raw(b)
          if (tr != null) {
            val prev = tr.src
            ms1(b) = Trace(tr.t, (id, b, prev) :: tr.st, tr.src, tr.root, tr.mid)
          }
        }

        val rest = shifts2(ms1, s, STARSS(r1, id))

        val out = Array.ofDim[Trace](n + 1)
        for (i <- 0 to n) {
          val t1 = ms1(i)
          out(i) = if (t1 != null) t1 else rest(i)
        }
        out
      }

    case NTIMESS(r1, k) =>
      if (k == 0) Array.ofDim[Trace](n + 1)
      else if (k == 1) shifts2(ms, s, r1)
      else {
        val ms1 = shifts2(ms, s, r1)
        var any = false
        for (i <- 0 to n) if (!any && ms1(i) != null) any = true
        if (!any) Array.ofDim[Trace](n + 1)
        else {
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

def mat2(r: RexpS, s: String): Marks2 =
  val n = s.length
  val ms0 = Array.ofDim[Trace](n + 1)
  ms0(0) = Trace(Nil, Nil, 0, 0, 0)
  shifts2(ms0, s, r)

def matcher2(r: RexpS, s: String): Boolean =
  val ms = mat2(r, s)
  ms(s.length) != null

def lexer2(r: Rexp, s: String): Val =
  val rs = intern(r)
  val n  = s.length
  val ms = mat2(rs, s)

  if (ms(n) == null) Invalid
  else {
    val tr = ms(n)

    val choiceMap = mutable.HashMap.empty[(Int, Int, Int), Int]   // (id,a,b) -> choice/split
    val starMap   = mutable.HashMap.empty[(Int, Int), Int]        // (starId,b) -> prev

    for ((id, a, b, c) <- tr.t.reverseIterator) {
      val k = (id, a, b)
      if (!choiceMap.contains(k)) choiceMap(k) = c
    }
    for ((sid, b, prev) <- tr.st.reverseIterator) {
      val k = (sid, b)
      if (!starMap.contains(k)) starMap(k) = prev
    }

    def backFast(r: RexpS, a: Int, b: Int): Val =
      r match
        case ONES =>
          if (a == b) Empty else Invalid

        case CHARS(c) =>
          if (b == a + 1 && a >= 0 && b <= s.length && s(a) == c) Chr(c) else Invalid

        case ALTS(r1, r2, id) =>
          choiceMap.get((id, a, b)) match
            case Some(0) =>
              backFast(r1, a, b) match
                case Invalid => Invalid
                case v       => Left(v)
            case Some(1) =>
              backFast(r2, a, b) match
                case Invalid => Invalid
                case v       => Right(v)
            case _ =>
              Invalid

        case SEQS(r1, r2, id) =>
          choiceMap.get((id, a, b)) match
            case Some(k) if k == b =>
              backFast(r1, a, b) match
                case Invalid => Invalid
                case v1      => Sequ(v1, rexps.mkeps(r2))

            case Some(k) if k == a =>
              backFast(r2, a, b) match
                case Invalid => Invalid
                case v2      => Sequ(rexps.mkeps(r1), v2)

            case Some(k) =>
              (backFast(r1, a, k), backFast(r2, k, b)) match
                case (Invalid, _) => Invalid
                case (_, Invalid) => Invalid
                case (v1, v2)     => Sequ(v1, v2)

            case None =>
              Invalid

        case STARSS(r1, id) =>
          if (a == b) Stars(Nil)
          else {
            var end  = b
            var vs   = List.empty[Val]
            var ok   = true

            while (ok && end != a) {
              starMap.get((id, end)) match
                case None =>
                  ok = false
                case Some(prev) =>
                  val v = backFast(r1, prev, end)
                  if (v == Invalid) ok = false
                  else {
                    vs = v :: vs
                    end = prev
                  }
            }

            if (!ok || end != a) Invalid else Stars(vs)
          }

        case _ =>
          Invalid

    backFast(rs, 0, n)
  }

@main
def tests() =
  val reg = ("aa") | ("a" ~ (ONE ~ "a"))
  val s   = "aa"
  val der = rebit.blexer(reg, s)
  val mar = lexer2(reg, s)
  println(s"Derivative Value=${der}")
  println(s"Marks2 Value=${mar}")
  println(s"Der==Mar: ${der == mar}")
  println("-" * 40)

  val reg2 = %("a" | "aa")
  val s2   = "aaa"
  val der2 = rebit.blexer(reg2, s2)
  val mar2 = lexer2(reg2, s2)
  println(s"Derivative Value=${der2}")
  println(s"Marks2 Value=${mar2}")
  println(s"Der==Mar: ${der2 == mar2}")
  println("-" * 40)

  val reg3 = (ONE | %("a"))
  val s3   = "a"
  val der3 = rebit.blexer(reg3, s3)
  val mar3 = lexer2(reg3, s3)
  println(s"Derivative Value=${der3}")
  println(s"Marks2 Value=${mar3}")
  println(s"Der==Mar: ${der3 == mar3}")
  println("-" * 40)

  val reg4 = ("a" | %("a"))
  val s4   = "a"
  val der4 = rebit.blexer(reg4, s4)
  val mar4 = lexer2(reg4, s4)
  println(s"Derivative Value=${der4}")
  println(s"Marks2 Value=${mar4}")
  println(s"Der==Mar: ${der4 == mar4}")
  println("-" * 40)

  val reg5 = (%(ONE) ~ ("a"))
  val s5   = "a"
  val der5 = rebit.blexer(reg5, s5)
  val mar5 = lexer2(reg5, s5)
  println(s"Derivative Value=${der5}")
  println(s"Marks2 Value=${mar5}")
  println(s"Der==Mar: ${der5 == mar5}")
  println("-" * 40)

  val reg6 = (%("a") | %("aa"))
  val s6   = "aa"
  val der6 = rebit.blexer(reg6, s6)
  val mar6 = lexer2(reg6, s6)
  println(s"Derivative Value=${der6}")
  println(s"Marks2 Value=${mar6}")
  println(s"Der==Mar: ${der6 == mar6}")
  println("-" * 40)

  val reg7 = ((ONE | "a") ~ %("a"))
  val s7   = "a"
  val der7 = rebit.blexer(reg7, s7)
  val mar7 = lexer2(reg7, s7)
  println(s"Derivative Value=${der7}")
  println(s"Marks2 Value=${mar7}")
  println(s"Der==Mar: ${der7 == mar7}")
  println("-" * 40)

  val reg8 = (ONE ~ "a") | ("a" ~ ONE)
  val s8   = "a"
  val der8 = rebit.blexer(reg8, s8)
  val mar8 = lexer2(reg8, s8)
  println(s"Derivative Value=${der8}")
  println(s"Marks2 Value=${mar8}")
  println(s"Der==Mar: ${der8 == mar8}")
  println("-" * 40)

  val reg9 = (("a" ~ ONE) | (ONE ~ "a")) ~ %("a")
  val s9   = "a"
  val der9 = rebit.blexer(reg9, s9)
  val mar9 = lexer2(reg9, s9)
  println(s"Derivative Value=${der9}")
  println(s"Marks2 Value=${mar9}")
  println(s"Der==Mar: ${der9 == mar9}")
  println("-" * 40)

  val reg10 = (("a" | "b") | "b")
  val s10   = "b"
  val der10 = rebit.blexer(reg10, s10)
  val mar10 = lexer2(reg10, s10)
  println(s"Derivative Value=${der10}")
  println(s"Marks2 Value=${mar10}")
  println(s"Der==Mar: ${der10 == mar10}")
  println("-" * 40)

  val reg11 = ("a" | ("ab" | "ba"))
  val s11   = "ab"
  val der11 = rebit.blexer(reg11, s11)
  val mar11 = lexer2(reg11, s11)
  println(s"Derivative Value=${der11}")
  println(s"Marks2 Value=${mar11}")
  println(s"Der==Mar: ${der11 == mar11}")
  println("-" * 40)

  val reg12 = ("a" | "ab") ~ ("b" | ONE)
  val s12   = "ab"
  val der12 = rebit.blexer(reg12, s12)
  val mar12 = lexer2(reg12, s12)
  println(s"Derivative Value=${der12}")
  println(s"Marks2 Value=${mar12}")
  println(s"Der==Mar: ${der12 == mar12}")
  println("-" * 40)


@main
def test4() =
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
def test5() =
  val reg13 = (("a")~ ONE )
  val s13   = "a"
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
def testall() =
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
