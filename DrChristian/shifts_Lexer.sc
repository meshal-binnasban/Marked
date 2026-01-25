import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.rexps, rexps._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit

type Marks = Set[Int]

case class Mark(m: Int, m0: Int)

/* def shifts(ms: Marks, s: String, r: RexpS): Set[Mark] =
  ms.flatMap(m0 => shifts1(m0, m0, s, r))
 */
def shifts(m: Int, m0: Int, s: String, r: RexpS): Set[Mark] = r match {
    case ZEROS => Set()
    case ONES  => Set()
    case CHARS(c) => if (m < s.length && s(m) == c) Set(Mark(m + 1, m)) else Set()
    case ALTS(r1, r2, _) => shifts(m, m0, s, r1) ++ shifts(m, m0, s, r2)

    case SEQS(r1, r2, _) =>
        val ms1Marks = shifts(m, m0, s, r1)
        val ks = ms1Marks.map(_.m)
        (nullableS(r1), nullableS(r2)) match {
            case (true, true)   => 
                (ks + m).flatMap(m0 => shifts(m0, m0, s, r2)) ++ ms1Marks
                //shifts(ks + m, s, r2) ++ ms1Marks
            case (true, false)  => 
                (ks + m).flatMap(m0 => shifts(m0, m0, s, r2))
                //shifts(ks + m, s, r2)
            case (false, true)  => 
                (ks ).flatMap(m0 => shifts(m0, m0, s, r2)) ++ ms1Marks
                //shifts(ks, s, r2) ++ ms1Marks
            case (false, false) => 
                (ks ).flatMap(m0 => shifts(m0, m0, s, r2))
                //shifts(ks, s, r2)
        }

    case STARSS(r1, id) =>
        val ms1Marks = shifts(m, m0, s, r1)
        val ks = ms1Marks.map(_.m)
        if (ks.isEmpty) Set()
        else ms1Marks ++ ks.flatMap(k => shifts(k, k, s, STARSS(r1, id)))

    case NTIMESS(r1, n) =>
        if (n == 0) Set()
        else if (n == 1) shifts(m, m0, s, r1)
        else {
        val ms1Marks = shifts(m, m0, s, r1)
        val ks = ms1Marks.map(_.m)
        if (ks.isEmpty) Set()
        else {
            val rest = ks.flatMap(k => shifts(k, k, s, NTIMESS(r1, n - 1)))
            if (nullableS(r1)) ms1Marks ++ rest else rest
        }
        }

    case ANDS(r1, r2) =>
        val a = shifts(m, m0, s, r1)
        val b = shifts(m, m0, s, r2)
        val bm = b.iterator.map(_.m).toSet
        a.filter(x => bm.contains(x.m))
}

def mat(r: Rexp, s: String): Set[Mark] = {
  val rs = intern(r)
  shifts(0, 0, s, rs)
  //shifts(Set(0), s, rs)
}

def matcher(r: Rexp, s: String): Boolean =
  if (s == "") nullable(r) else mat(r, s).exists(_.m == s.length)

def lexer(r: Rexp, s: String): Val =
  if (s == "") {
    if (nullable(r))
    {
        println("Not Implemented Yet")
        Invalid
    }else Invalid

  } else {
    println("Not Implemented Yet")
    Invalid
  }

@main
def tests() = {
  val reg = ("aa") | ("a" ~ (ONE ~ "a"))
  val s   = "aa"
  println(s"1- $s: r=\n${pp(reg)}  ${mat(reg, s)} ")
  println(s"Derivative Value=${rebit.blexer(reg, s)}")
  println(lexer(reg, s))
  println("-"*40)

  val reg2 = %("a"|"aa")
  val s2   = "aaa"
  println(s"2- $s2: r=\n${pp(reg2)}  ${mat(reg2, s2)} ")
  println(s"Derivative Value=${rebit.blexer(reg2, s2)}")
  println(lexer(reg2, s2))
  println("-"*40)

  val reg3 = (ONE | %("a"))
  val s3   = "a"
  println(s"3- $s3: r=\n${pp(reg3)}  ${mat(reg3, s3)} ")
  println(s"Derivative Value=${rebit.blexer(reg3, s3)}")
  println(lexer(reg3, s3))
  println("-"*40)

  val reg4 = ("a"| %("a") )
  val s4   = "a"
  println(s"4- $s4: r=\n${pp(reg4)}  ${mat(reg4, s4)} ")
  println(s"Derivative Value=${rebit.blexer(reg4, s4)}")
  println(lexer(reg4, s4))
  println("-"*40)

  val reg5 = ( %(ONE) ~ ("a") )
  val s5   = "a"
  println(s"5- $s5: r=\n${pp(reg5)}  ${mat(reg5, s5)} ")
  println(s"Derivative Value=${rebit.blexer(reg5, s5)}")
  println(lexer(reg5, s5))
  println("-"*40)

  val reg6 = ( %("a") | %("aa") )
  val s6   = "aa"
  println(s"6- $s6: r=\n${pp(reg6)}  ${mat(reg6, s6)} ")
  println(s"Derivative Value=${rebit.blexer(reg6, s6)}")
  println(lexer(reg6, s6))
  println("-"*40)

  val reg7 = ( ( ONE | "a" ) ~ %("a") )
  val s7   = "a"
  println(s"7- $s7: r=\n${pp(reg7)}  ${mat(reg7, s7)} ")
  println(s"Derivative Value=${rebit.blexer(reg7, s7)}")
  println(lexer(reg7, s7))
  println("-"*40)

  val reg8 = ( ONE ~ "a" ) | ("a" ~ ONE )
  val s8   = "a"
  println(s"8- $s8: r=\n${pp(reg8)}  ${mat(reg8, s8)} ")
  println(s"Derivative Value=${rebit.blexer(reg8, s8)}")
  println(lexer(reg8, s8))
  println("-"*40)

  val reg9 = (( "a" ~ ONE ) | ( ONE ~ "a" )) ~ %("a")
  val s9   = "a"
  println(s"9- $s9: r=\n${pp(reg9)}  ${mat(reg9, s9)} ")
  println(s"Derivative Value=${rebit.blexer(reg9, s9)}")
  println(lexer(reg9, s9))
  println("-"*40)
}

@main
def test1() = {
  val reg9 = (ONE | "a")  ~  "a"
  val s9   = "aa"
  println(s"9- $s9: r=\n${pp(reg9)}  ${mat(reg9, s9)} ")
  println(s"Derivative Value=${rebit.blexer(reg9, s9)}")
  println(lexer(reg9, s9))
  println("-"*40)
}

@main
def test2() = {
  val reg2 = %("a"|"aa")
  val s2   = "aaaaa"
  println(s"2- $s2: r=\n${pp(reg2)}  ${mat(reg2, s2)} ")
  println(s"Derivative Value=${rebit.blexer(reg2, s2)}")
  println(lexer(reg2, s2))
  println("-"*40)
}
