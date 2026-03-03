





import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.re_bitrev3
import $file.rexp, rexp._
import scala.language.implicitConversions



def mat(r: Rexp, s: String): Frontier =
  shiftF(List(F(0, 0, Trace(Nil))), s, r)

def matcher(r: Rexp, s: String): Boolean =
  s match {
    case "" => nullable(r)
    case _ =>
      val fs = mat(r, s)
      fs.exists(_.m == s.length)
  }

def lexer(r: Rexp, s: String, debug: Boolean = false): Val =
  s match {
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