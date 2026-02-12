import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.rexps, rexps._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit
import scala.collection.mutable

type Marks = Set[Int]

val trace = mutable.HashMap.empty[(Int, Int), mutable.HashMap[Int, Int]]
val starTrace = mutable.HashMap.empty[(Int, Int), Int]   // (starId, end m) -> prev m

def record(id: Int, m: Int, start: Int, choice: Int): Unit = 
    trace.getOrElseUpdate((id, m), mutable.HashMap.empty)(start) = choice


def shifts(ms: Marks, s: String, r: RexpS, seen: Marks = Set.empty): Marks =
  r match {
    case ZEROS => Set.empty
    case ONES  => Set.empty

   // case CHARS(c, id) => for { m <- ms if m < s.length && s(m) == c } yield (m + 1)
    case CHARS(c) => for { m <- ms if m < s.length && s(m) == c } yield (m + 1)

    case ALTS(r1, r2, id) =>
      ms.flatMap { start =>
        val ms1 = shifts(Set(start), s, r1)
        val ms2 = shifts(Set(start), s, r2)
        ms1.foreach { m =>
          val cur = trace.get((id, m)).flatMap(_.get(start))
          if (cur.isEmpty) record(id, m, start, 0)
        }
        ms2.foreach { m =>
          if (!ms1.contains(m)) {
            val cur = trace.get((id, m)).flatMap(_.get(start))
            if (cur.isEmpty) record(id, m, start, 1)
          }
        }
        ms1 ++ ms2
      }

    case SEQS(r1, r2, id) =>
      val n1 = nullableS(r1)
      val n2 = nullableS(r2)
      ms.flatMap { start =>
        val ms1 = shifts(Set(start), s, r1).toList.sorted(Ordering.Int.reverse)
        val skipR2 = if (n2) {
            ms1.foreach { m => 
                val cur = trace.get((id, m)).flatMap(_.get(start))
                if (cur.isEmpty) record(id, m, start, m)
                }
            ms1
          } else Set.empty

        val both = ms1.flatMap { split =>
            val ms2 = shifts(Set(split), s, r2)
            ms2.foreach { m =>
              val cur = trace.get((id, m)).flatMap(_.get(start))
              if (cur.isEmpty) record(id, m, start, split)
            }
            ms2
          }.toSet

        val skipR1 = if (n1) {
            val ms2 = shifts(Set(start), s, r2)
            ms2.foreach { m =>
                val cur = trace.get((id, m)).flatMap(_.get(start))
                if (cur.isEmpty) record(id, m, start, start)
            }
            ms2
            } else Set.empty

        skipR2 ++ both ++ skipR1
      }

    case STARSS(r, id) =>
        def go(ms: Marks, seen: Marks): Marks = {
            val msn = ms -- seen
            if (msn.isEmpty) Set.empty
            else {
                val ms1L =
                msn.toList.sorted(Ordering.Int.reverse).flatMap { start =>
                    val result = shifts(Set(start), s, r).toList.sorted(Ordering.Int.reverse)
                    result.foreach { m =>
                        if (!starTrace.contains((id, m))) starTrace((id, m)) = start
                    }
                    result
                }

                val ms1 = ms1L.toSet
                ms1 ++ go(ms1, seen ++ msn)
            }
        }

        go(ms, Set.empty)

    case NTIMESS(r1, n) =>
      if (n == 0) Set.empty
      else if (n == 1) shifts(ms, s, r1)
      else {
        val ms1 = shifts(ms, s, r1)
        if (ms1.isEmpty) Set.empty
        else {
          val ms2 = shifts(ms1, s, NTIMESS(r1, n - 1))
          if (nullableS(r1)) ms1 ++ ms2 else ms2
        }
      }

    case ANDS(r1, r2) =>
      shifts(ms, s, r1).intersect(shifts(ms, s, r2))
  }

def back(r: RexpS, s: String, a: Int, b: Int): Val =
  r match {
    case ONES =>if (a == b) Empty else Invalid

    //case CHARS(c, id) => if (b == a + 1 && a >= 0 && b <= s.length && s(a) == c) Chr(c) else Invalid
    case CHARS(c) => if (b == a + 1 && a >= 0 && b <= s.length && s(a) == c) Chr(c) else Invalid
    case ALTS(r1, r2, id) =>
      trace.get((id, b)).flatMap(_.get(a)) match {
        case Some(0) =>
          back(r1, s, a, b) match {
            case Invalid => Invalid
            case v       => Left(v)
          }
        case Some(1) =>
          back(r2, s, a, b) match {
            case Invalid => Invalid
            case v       => Right(v)
          }
        case _ => Invalid
      }

    case SEQS(r1, r2, id) =>
      trace.get((id, b)).flatMap(_.get(a)) match {
        case Some(k) if k == b =>
          back(r1, s, a, b) match {
            case Invalid => Invalid
            case v1      => Sequ(v1, rexps.mkeps(r2))
          }

        case Some(k) if k == a =>
          back(r2, s, a, b) match {
            case Invalid => Invalid
            case v2      => Sequ(rexps.mkeps(r1), v2)
          }

        case Some(k) =>
          (back(r1, s, a, k), back(r2, s, k, b)) match {
            case (Invalid, _) => Invalid
            case (_, Invalid) => Invalid
            case (v1, v2)     => Sequ(v1, v2)
          }

        case None => Invalid
      }
    case STARSS(r, id) =>
        if (a == b) Stars(Nil)// empty if nullable r?
        else {
            starTrace.get((id, b)) match {
            case None => Invalid
            case Some(prev) =>
                (back(STARSS(r, id), s, a, prev), back(r, s, prev, b)) match {
                case (Stars(vs), v) if v != Invalid => Stars((v::vs).reverse)
                case _                              => Invalid
                }
            }
        }

    case _ => Invalid
  }



def mat(r: RexpS, s: String): Boolean = {
  trace.clear()
  starTrace.clear()
  shifts(Set(0), s, r).contains(s.length)
}

def matcher(r: RexpS, s: String): Val = {
  trace.clear()
  starTrace.clear()
  val ms = shifts(Set(0), s, r)
  if (ms.contains(s.length)) back(r, s, 0, s.length) else Invalid
}

def lexer(r: Rexp, s: String, debug: Boolean = false): Val = {
  trace.clear()
  starTrace.clear()

  val rs = intern(r)
  val ms = shifts(Set(0), s, rs)

  if(debug) {
  println(s"RexpS:\n${ppp(rs)}")
  println(s"s=$s")
  println(s"marks=$ms")

  println("\n(id,m) : from -> choice")
  trace.toList
    .sortBy { case ((id, m), _) => (id, m) }
    .foreach { case ((id, m), mp) =>
      val pairs = mp.toList.map { case (start, choice) => s"$start -> $choice" }.mkString(", ")
      println(s"($id,$m): $pairs")
    }

  println("\n(Star id, end) : previous start")
  starTrace.toList
    .sortBy { case ((id, end), _) => (id, end) }
    .foreach { case ((id, end), start) =>
      println(s"($id,$end): $start")
    }

  }

  if (ms.isEmpty) Invalid
  else {
    val b = ms.max
    back(rs, s, 0, b)
  }
}




@main
def tests() = {
  val reg = ("aa") | ("a" ~ (ONE ~ "a"))
  val s   = "aa"
  val der=rebit.blexer(reg, s)
  val mar=lexer(reg, s)
  println(s"Derivative Value=${der}")
  println(s"Marks Value=${mar}")
  println(s"Der==Mar: ${der==mar}")
  println("-"*40)

  val reg2 = %("a"|"aa")
  val s2   = "aaa"
  val der2=rebit.blexer(reg2, s2)
  val mar2=lexer(reg2, s2)
  println(s"Derivative Value=${der2}")
  println(s"Marks Value=${mar2}")
  println(s"Der==Mar: ${der2==mar2}")
  println("-"*40)

  val reg3 = (ONE | %("a"))
  val s3   = "a"
  val der3=rebit.blexer(reg3, s3)
  val mar3=lexer(reg3, s3)
  println(s"Derivative Value=${der3}")
  println(s"Marks Value=${mar3}")
  println(s"Der==Mar: ${der3==mar3}")
  println("-"*40)

  val reg4 = ("a"| %("a") )
  val s4   = "a"
  val der4=rebit.blexer(reg4, s4)
  val mar4=lexer(reg4, s4)
  println(s"Derivative Value=${der4}")
  println(s"Marks Value=${mar4}")
  println(s"Der==Mar: ${der4==mar4}")
  println("-"*40)

  val reg5 = ( %(ONE) ~ ("a") )
  val s5   = "a"
  val der5=rebit.blexer(reg5, s5)
  val mar5=lexer(reg5, s5)
  println(s"Derivative Value=${der5}")
  println(s"Marks Value=${mar5}")
  println(s"Der==Mar: ${der5==mar5}")
  println("-"*40)

  val reg6 = ( %("a") | %("aa") )
  val s6   = "aa"
  val der6=rebit.blexer(reg6, s6)
  val mar6=lexer(reg6, s6)
  println(s"Derivative Value=${der6}")
  println(s"Marks Value=${mar6}")
  println(s"Der==Mar: ${der6==mar6}")
  println("-"*40)

  val reg7 = ( ( ONE | "a" ) ~ %("a") )
  val s7   = "a"
  val der7=rebit.blexer(reg7, s7)
  val mar7=lexer(reg7, s7)
  println(s"Derivative Value=${der7}")
  println(s"Marks Value=${mar7}")
  println(s"Der==Mar: ${der7==mar7}")
  println("-"*40)

  val reg8 = ( ONE ~ "a" ) | ("a" ~ ONE )
  val s8   = "a"
  val der8=rebit.blexer(reg8, s8)
  val mar8=lexer(reg8, s8)
  println(s"Derivative Value=${der8}")
  println(s"Marks Value=${mar8}")
  println(s"Der==Mar: ${der8==mar8}")
  println("-"*40)

  val reg9 = (( "a" ~ ONE ) | ( ONE ~ "a" )) ~ %("a")
  val s9   = "a"
  val der9=rebit.blexer(reg9, s9)
  val mar9=lexer(reg9, s9)
  println(s"Derivative Value=${der9}")
  println(s"Marks Value=${mar9}")
  println(s"Der==Mar: ${der9==mar9}")
  println("-"*40)

  val reg10 = ( ("a" | "b") | "b") 
  val s10   = "b"
  val der10=rebit.blexer(reg10, s10)
  val mar10=lexer(reg10, s10)
  println(s"Derivative Value=${der10}")
  println(s"Marks Value=${mar10}")
  println(s"Der==Mar: ${der10==mar10}")
  println("-"*40)

  val reg11 =  ( "a" | ("ab" | "ba") )
  val s11   = "ab"
  val der11=rebit.blexer(reg11, s11)
  val mar11=lexer(reg11, s11)
  println(s"Derivative Value=${der11}")
  println(s"Marks Value=${mar11}")
  println(s"Der==Mar: ${der11==mar11}")
  println("-"*40)

  val reg12 =  ( "a" |  "ab" ) ~ ("b" | ONE) 
  val s12   = "ab"
  val der12=rebit.blexer(reg12, s12)
  val mar12=lexer(reg12, s12,true)
  println(s"Derivative Value=${der12}")
  println(s"Marks Value=${mar12}")
  println(s"Der==Mar: ${der12==mar12}")
  println("-"*40)


}

@main
def test1() = {
  val reg2 = %("a"| "aa")
  val s2   = "aaa"

  //println(s"2- $s2: r=\n${pp(reg2)}  ${mat(reg2, s2)} ")
  println(s"Derivative Value=${rebit.blexer(reg2, s2)}")
  println(lexer(reg2, s2))
  println("-"*40)
  //println(lexer2(reg2, s2))
}

@main
def test2() = {
  val reg11 =  ( "a" | ("ab" | "ba") )
  val s11   = "ab"
  println(s"11- $s11: r=\n${pp(reg11)} ")
  println(s"Derivative Value=${rebit.blexer(reg11, s11)}")
  println(lexer(reg11, s11))
  println("-"*40)
}

@main
def test3() = {
  val reg12 =  %("a" | ZERO)
  val s12   = "aaa"
  println(s"12- $s12: r=\n${pp(reg12)} ")
  println(s"Derivative Value=${rebit.blexer(reg12, s12)}")
  println(lexer(reg12, s12))
  println("-"*40)
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
    (2, cs => SEQ(cs(0), cs(1)))
  )
  val alphabet = LazyList('a', 'b')

  for (i <- 0L to 1_000_000_000L) {
    val r = decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- generate_up_to(alphabet)(20)(r).take(19) if s != "") {
      val vm  = lexer(r, s)
      val vb  = rebit.blexer(r, s)
      val res = vm == vb
      if (!res) {
        println(s"$r and $s")
        println(s"vm=$vm vb=$vb")
        System.exit(1)
      }
    }
  }
}

