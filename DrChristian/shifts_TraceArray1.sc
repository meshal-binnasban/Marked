import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.rexps, rexps._
import $file.rebit
import scala.collection.mutable
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._

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
  def run(reg: Rexp, s: String): Unit =
    val mar = lexer2(reg, s, true)
    val der = rebit.blexer(reg, s)
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
    ((("a" | ONE) ~ "a") ~ %("a"),"aaa")

  )

  cases.zipWithIndex.foreach { case ((reg, s), idx) =>
  val i = idx + 1
  println(s"$i-")
  run(reg, s)
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





/* 
def shifts3(marks: List[Int], trace: Array[Trace], s: String, r: RexpS): (List[Int], Array[Trace]) = {
  val n = s.length

  r match {
    case ZEROS =>
      (Nil, new Array[Trace](n + 1))

    case ONES =>
      (Nil, new Array[Trace](n + 1))

    case CHARS(c) =>
      val outTrace = new Array[Trace](n + 1)
      var outMarks: List[Int] = Nil

      for (m <- marks) {
        if (m < n && s(m) == c) {
          val b = m + 1
          outTrace(b) = trace(m)
          outMarks = b :: outMarks
        }
      }

      (outMarks.reverse, outTrace)

    case ALTS(r1, r2, id) =>
      val outTrace = new Array[Trace](n + 1)
      var outMarks: List[Int] = Nil

      for (a <- marks) {
        val (leftMarks, leftTrace) = shifts3(List(a), trace, s, r1)
        for (b <- leftMarks) {
          if (outTrace(b) == null) {
            val trb = leftTrace(b)
            outTrace(b) = Trace((id, a, b, 0) :: trb.t, trb.st)
            outMarks = b :: outMarks
          }
        }

        val (rightMarks, rightTrace) = shifts3(List(a), trace, s, r2)
        for (b <- rightMarks) {
          if (outTrace(b) == null) {
            val trb = rightTrace(b)
            outTrace(b) = Trace((id, a, b, 1) :: trb.t, trb.st)
            outMarks = b :: outMarks
          }
        }
      }

      (outMarks.reverse, outTrace)

    case SEQS(r1, r2, id) =>
      val outTrace = new Array[Trace](n + 1)
      var outMarks: List[Int] = Nil

      val n1 = nullableS(r1)
      val n2 = nullableS(r2)

      for (start <- marks) {

        val (ms1Marks, ms1Trace) = shifts3(List(start), trace, s, r1)

        if (n2) {
          for (b <- ms1Marks.sorted(Ordering.Int.reverse)) {
            if (outTrace(b) == null) {
              val tr1 = ms1Trace(b)
              outTrace(b) = Trace((id, start, b, b) :: tr1.t, tr1.st)
              outMarks = b :: outMarks
            }
          }
        }

        for (split <- ms1Marks.sorted(Ordering.Int.reverse)) {
          val (ms2Marks, ms2Trace) = shifts3(List(split), ms1Trace, s, r2)

          for (b <- ms2Marks) {
            if (outTrace(b) == null) {
              val tr2 = ms2Trace(b)
              outTrace(b) = Trace((id, start, b, split) :: tr2.t, tr2.st)
              outMarks = b :: outMarks
            }
          }
        }

        if (n1) {
          val (ms2Marks, ms2Trace) = shifts3(List(start), trace, s, r2)

          for (b <- ms2Marks) {
            if (outTrace(b) == null) {
              val tr2 = ms2Trace(b)
              outTrace(b) = Trace((id, start, b, start) :: tr2.t, tr2.st)
              outMarks = b :: outMarks
            }
          }
        }
      }

      (outMarks.sorted, outTrace)

    case _ =>
      ???
  }
}
 */