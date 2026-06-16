//| mainClass: Main
//| scalaVersion: 3.8.3
//| scalacOptions: ["-deprecation", "-feature", "-language:implicitConversions" , "-nowarn"]
//| mvnDeps: 
//|   - org.scala-lang:scala3-library_3:3.8.3
//|   - org.scala-lang.modules::scala-parallel-collections:1.2.0
//| moduleDeps: 
//|   - rexp.scala
//|   - enumerate.scala
//|   - regenerate.scala
//|   - re_bitrev3.scala
//|   - Generators.scala
//> using scala 3.8.3
//> using dep org.scala-lang.modules::scala-parallel-collections:1.2.0
//> using file rexp.scala
//> using file enumerate.scala
//> using file regenerate.scala
//> using file re_bitrev3.scala
//> using file Generators.scala

import Rexp._

val reg=(ONE | "a") ~ ("aa" | "a")
val s="aa"

type Mark = (Int, Bits)
type Marks = List[Mark]

def mat1(r: Rexp, s: String): Array[Bits] = {
  val tArray = new Array[Bits](s.length + 1)

  def shifts2(ms: Marks, r: Rexp): Marks =
    r match {
      case ZERO => Nil
      case ONE => ms
      case CHAR(c) =>
        for ((m, bs) <- ms if m < s.length && s(m) == c) yield {
          if (tArray(m + 1) == null) tArray(m + 1) = bs
          (m + 1, bs)
        }

      case ALT(r1, r2) =>
        val ms1 = shifts2(ms.map { case (m, bs) => (m, bs:+ Bit.Z) }, r1)
        val ms2 = shifts2(ms.map { case (m, bs) => (m, bs:+ Bit.S) }, r2)
        val ms3= ms1 ::: ms2

        /* for((m,bs) <- ms3 if(tArray(m)== null)){
          tArray(m)=bs
        } */
        ms3

      case SEQ(r1, r2) => 
        val ms3=shifts2(shifts2(ms, r1), r2)
        /* for((m,bs) <- ms3 if(tArray(m)== null)){
          tArray(m)=bs
        } */
        ms3

      case STAR(r) =>
        val ms0 = ms.map { case (m, bs) => (m, bs:+ Bit.Ss) }
        val ms1 = shifts2(ms.map { case (m, bs) => (m, bs:+ Bit.Zz) }, r)
            .filterNot { case (m, _) => ms.exists(_._1 == m) }

        if (ms1.isEmpty) 
          /* for((m,bs) <- ms0 if(tArray(m)== null)){
          tArray(m)=bs
          } */
          ms0
        else 
          val ms4=ms0 ::: shifts2(ms1, STAR(r))
          /* for((m,bs) <- ms4 if(tArray(m)== null)){
          tArray(m)=bs
          } */
          ms4
    }

  shifts2(List((0, Nil)), r)
  tArray
}

def matcher1(r: Rexp, s: String): Boolean =
  if (s == "") nullable(r)
  else mat1(r, s)(s.length) != null

def lex1(r: Rexp, s: String): Bits =
  if (s == "") {
    if (nullable(r)) mkepsBits(r)
    else throw new Exception("no match")
  } else {
    mat1(r, s)(s.length) match {
      case null => throw new Exception("no match")
      case bs   => bs
    }
  }

def lexer1(r: Rexp, s: String): Val =
  decode(r, lex1(r, s))

def testMat1(): Unit = {
  val examples = List[(Rexp, String)](
    ((ONE | "a") ~ ("aa" | "a"), "aa"),

    ((ONE | "a") ~ ("ab" | "b"), "ab"),
    ((ONE | "c") ~ (("c" ~ "c") | "c"), "cc"),
    ("aa" | ("a" ~ (ONE ~ "a")), "aa"),
    ((ONE ~ "a") | ("a" ~ ONE), "a"),
    ((("a" | "b") | "b"), "b"),
    ("a" | ("ab" | "ba"), "ab"),
    ((("a" | "ab") ~ ("b" | ONE)), "ab"),
    ("abc", "abc"),
    ((("a" | ("a" ~ "a")) ~ ("a" | ("a" ~ "a"))), "aaa"),
    (((("a" ~ "a") | "a") ~ ("a" | ("a" ~ "a"))), "aaa"),
    ((("a" | ("a" ~ "a")) ~ ("a" | ("a" ~ "a"))), "aaa"),
    (((("a" | "c") ~ ("c" ~ "b")) | ((ZERO ~ ONE) ~ ONE)), "acb"),
    (ONE | "a", "a"),

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
    (("b" ~ ONE | %("a")) ~ %("a" | "b"), "aab"),
    (%("b" | %("a")), "bab")
  )

  var inequality = 0

  examples.zipWithIndex.foreach { case ((r, s), i) =>
    val marks = lex1(r, s)
    val der = blex_simp(internalise(r), s.toList).reverse

    if (marks != der) inequality += 1

    println(s"Test ${i + 1}")
    println(s"Regex: $r")
    println(s"Input: $s")
    println(s"Marks: ${marks.mkString("(", ",", ")")}")
    println(s"Derivative: ${der.mkString("(", ",", ")")}")
    println(s"Equal: ${marks == der}")
    println()
  }

  println(s"Inequality: $inequality / ${examples.length}")
}


def mat2(r: Rexp, s: String): Array[Bits] = {
  val tArray = new Array[Bits](s.length + 1)

  def shifts2(ms: Marks, r: Rexp, flag:Boolean=true): Marks =

    r match {
      case ZERO => Nil
      case ONE => ms
      case CHAR(c) =>
        for ((m, bs) <- ms if m < s.length && s(m) == c) yield {
          if (tArray(m + 1) == null && flag) tArray(m + 1) = bs
          (m + 1, bs)
        }
        
/*       case ALT(r1, r2) =>
        val mss= ms.flatMap { case (m, bs) =>
          val ms1 = shifts2(List((m, bs:+Bit.Z )), r1)
          val ms2 = shifts2(List((m, bs:+Bit.S )), r2)
          ms1 ::: ms2
        }
        mss */

      case ALT(r1, r2) =>
        val ms1 = shifts2(ms.map { case (m, bs) => (m, bs:+ Bit.Z) }, r1)
        val ms2 = shifts2(ms.map { case (m, bs) => (m, bs:+ Bit.S) }, r2)
        ms1 ::: ms2

      case SEQ(r1, r2) => shifts2(shifts2(ms, r1,false).sortBy(_._1).reverse, r2,true)

      case STAR(r) =>
        val ms0 = ms.map { case (m, bs) => (m, bs:+ Bit.Ss) }
        val ms1 = shifts2(ms.map { case (m, bs) => (m, bs:+ Bit.Zz) }, r)
            .filterNot { case (m, _) => ms.exists(_._1 == m) }

        if (ms1.isEmpty) ms0
        else ms0 ::: shifts2(ms1, STAR(r))
    }

  shifts2(List((0, Nil)), r)
  tArray
}

def matcher2(r: Rexp, s: String): Boolean =
  if (s == "") nullable(r)
  else mat1(r, s)(s.length) != null

def lex2(r: Rexp, s: String): Bits =
  if (s == "") {
    if (nullable(r)) mkepsBits(r)
    else throw new Exception("no match")
  } else {
    mat2(r, s)(s.length) match {
      case null => throw new Exception("no match")
      case bs   => bs
    }
  }

def lexer2(r: Rexp, s: String): Val =
  decode(r, lex2(r, s))


def testMat2(): Unit = {
  val examples = List[(Rexp, String)](
    ((ONE | "a") ~ ("aa" | "a"), "aa"),

    ((ONE | "a") ~ ("ab" | "b"), "ab"),
    ((ONE | "c") ~ (("c" ~ "c") | "c"), "cc"),
    ("aa" | ("a" ~ (ONE ~ "a")), "aa"),
    ((ONE ~ "a") | ("a" ~ ONE), "a"),
    ((("a" | "b") | "b"), "b"),
    ("a" | ("ab" | "ba"), "ab"),
    ((("a" | "ab") ~ ("b" | ONE)), "ab"),
    ("abc", "abc"),
    ((("a" | ("a" ~ "a")) ~ ("a" | ("a" ~ "a"))), "aaa"),
    (((("a" ~ "a") | "a") ~ ("a" | ("a" ~ "a"))), "aaa"),
    ((("a" | ("a" ~ "a")) ~ ("a" | ("a" ~ "a"))), "aaa"),
    (((("a" | "c") ~ ("c" ~ "b")) | ((ZERO ~ ONE) ~ ONE)), "acb"),
    (ONE | "a", "a"),

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
    (("b" ~ ONE | %("a")) ~ %("a" | "b"), "aab"),
    (%("b" | %("a")), "bab")
  )

  var inequality = 0

  examples.zipWithIndex.foreach { case ((r, s), i) =>
    val marks = lex2(r, s)
    val der = blex_simp(internalise(r), s.toList).reverse

    if (marks != der) inequality += 1

    println(s"Test ${i + 1}")
    println(s"Regex: $r")
    println(s"Input: $s")
    println(s"Marks: ${marks.mkString("(", ",", ")")}")
    println(s"Derivative: ${der.mkString("(", ",", ")")}")
    println(s"Equal: ${marks == der}")
    println()
  }

  println(s"Inequality: $inequality / ${examples.length}")
}




/* 
type markC = (Int, Bits, Option[Int], Option[Boolean])
type marksC = List[markC]

def mat3(r: Rexp, s: String): Array[(Bits, Option[Int], Option[Boolean])] = {
  val tArray = new Array[(Bits, Option[Int], Option[Boolean])](s.length + 1)

  def shifts2(ms: marksC, r: Rexp, flag: Boolean): marksC =
    r match {
      case ZERO => Nil
      case ONE =>
        for ((m, bs, p, colour) <- ms) yield {
          if (flag)
            tArray(m) match {
              case null => tArray(m) = (bs, p, colour)
              case (_, Some(p0), _) if p.exists(_ > p0) => tArray(m) = (bs, p, colour)
              case (_, p0, Some(false)) if p == p0 && colour == Some(true) => tArray(m) = (bs, p, colour)
              case _ =>
            }
          (m, bs, p, colour)
        }
      case CHAR(c) =>
        for ((m, bs, p, colour) <- ms if m < s.length && s(m) == c) yield {
          if (flag)
            tArray(m + 1) match {
              case null => tArray(m + 1) = (bs, p, colour)
              case (_, Some(p0), _) if p.exists(_ > p0) => tArray(m + 1) = (bs, p, colour)
              case (_, p0, Some(false)) if p == p0 && colour == Some(true) => tArray(m + 1) = (bs, p, colour)
              case _ =>
            }
          (m + 1, bs, p, colour)
        }

      case ALT(r1, r2) =>
        val ms1 = shifts2( ms.map {
                                    case (m, bs, p, None) => (m, bs :+ Bit.Z, p, Some(true))
                                    case (m, bs, p, colour) => (m, bs :+ Bit.Z, p, colour)},
                          r1, flag)
        val ms2 = shifts2( ms.map {
                                    case (m, bs, p, None) => (m, bs :+ Bit.S, p, Some(false))
                                    case (m, bs, p, colour) => (m, bs :+ Bit.S, p, colour)},
                          r2,
                          flag)
        ms1 ::: ms2
      
      case SEQ(r1, r2) =>
        val ms1 = shifts2(ms, r1, false)
        val ms2 = if (flag) ms1.map {
                              case (m, bs, None, colour) => (m, bs, Some(m), colour)
                              case (m, bs, p, colour) => (m, bs, p, colour)
                                    }else ms1
        shifts2(ms2, r2, flag)
      case SEQ(r1, r2) =>
        val ms1 = shifts2(ms, r1, false)
        shifts2( ms1.map {
                          case (m, bs, None, colour) => (m, bs, Some(m), colour)
                          case (m, bs, p, colour) => (m, bs, p, colour)},
                 r2,
                 flag)

      case STAR(r) =>
        val ms0 = ms.map { case (m, bs, p, colour) => (m, bs :+ Bit.Ss, p, colour) }
        val ms1 = shifts2( ms.map { case (m, bs, p, colour) => (m, bs :+ Bit.Zz, p, colour) },
                           r,
                           flag).filterNot { case (m, _, _, _) => ms.exists(_._1 == m) }
        if (ms1.isEmpty) ms0 else ms0 ::: shifts2(ms1, STAR(r), flag)
    }

  val ms1=shifts2(List((0, Nil, None, None)), r, true)
  println(ms1)
  tArray
} */

import Val._

enum Trace {
  case Alt(m: Int, left: Boolean)
  case StarT(m: Int, cont:Boolean)
  case StarTT(cont: Boolean)
  case SeqD
}
import Trace._
//case class AltTrace(m: Int, left: Boolean)

type markC = (Int, List[Trace])
type marksC = List[markC]

def mat3(r: Rexp, s: String): Array[(Boolean, List[Trace])] = {
  val tArray = new Array[(Boolean, List[Trace])](s.length + 1)
  def shifts2(ms: marksC, r: Rexp, flag:Boolean=true): marksC =
    r match {
      case ZERO => Nil
      case ONE => 
        for ((m, trace) <- ms) yield {
          if (tArray(m) == null) tArray(m) = (true, trace)
          if (flag)
            tArray(m)._2 match {
              case Nil => tArray(m) = (true, trace)
              case trace0 if compareTrace(trace.reverse, trace0.reverse) => tArray(m) = (true, trace)
              case _ =>
            }    
          (m, trace)
        }
      case CHAR(c) =>
        for ((m, trace) <- ms if m < s.length && s(m) == c) yield {
          if (tArray(m + 1) == null) tArray(m + 1) = (true, Nil)

          if (flag)
            tArray(m + 1)._2 match {
              case Nil => tArray(m + 1) = (true, trace)
              case trace0 if compareTrace(trace.reverse, trace0.reverse) => tArray(m + 1) = (true, trace)
              case _ =>
            }
            
          (m + 1, trace)
        }
      case ALT(r1, r2) =>
        val ms1 = shifts2(ms, r1, false).map { case (m, trace) => (m, Alt(m, true) :: trace) }
        val ms2 = shifts2(ms, r2, false).map { case (m, trace) => (m, Alt(m, false) :: trace) }
        val ms3 = ms1 ::: ms2
        if (flag)
          for ((m, trace) <- ms3)
            tArray(m)._2 match {
              case Nil => tArray(m) = (tArray(m)._1, trace)
              case trace0 if compareTrace(trace.reverse, trace0.reverse) => tArray(m) = (tArray(m)._1, trace)
              case _ =>
            }
        

        ms3
      case SEQ(r1, r2) => shifts2(shifts2(ms, r1, false), r2, flag)

      case STAR(r) =>
        val ms0 = ms.map { case (m, trace) => (m, StarT(m,false) :: trace) }
        val ms1 = shifts2(ms0, r, false).filterNot { case (m, _) => ms.exists(_._1 == m) }
          .map { case (m, trace) => (m, StarT(m,true) :: trace) }

        val ms2 = if (ms1.isEmpty) ms0 else ms0 ::: shifts2(ms1, STAR(r), false)

        if (flag)
          for ((m, trace) <- ms2)
            tArray(m)._2 match {
              case Nil => tArray(m) = (tArray(m)._1, trace)
              case trace0 if compareTrace(trace.reverse, trace0.reverse) => tArray(m) = (tArray(m)._1, trace)
              case _ =>
            }

        ms2
      case NTIMES(r, n) =>
        if (n == 0) ms
        else shifts2(shifts2(ms, r), NTIMES(r, n - 1))
    }

  tArray(0) = (true, Nil)
  val ms = shifts2(List((0, Nil)), r)

  /* println("Marks:")
  for ((m, trace) <- ms)
    println(s"m=$m, trace=${trace}")

  println("Array:")
  for (m <- tArray.indices)
    println(s"$m: ${tArray(m)}") */

  tArray
}

def compareTrace(ts1: List[Trace], ts2: List[Trace]): Boolean =
  (ts1, ts2) match {
    //case (StarT(_, _) :: rest1, _) => compareTrace(rest1, ts2)
    //case (_, StarT(_, _) :: rest2) => compareTrace(ts1, rest2)
    case (StarTT(_) :: rest1, _) => compareTrace(rest1, ts2)
    case (_, StarTT(_) :: rest2) => compareTrace(ts1, rest2)
    case (Alt(m1, left1) :: rest1, Alt(m2, left2) :: rest2) =>
      if (m1 != m2) m1 > m2
      else if (left1 != left2) left1
      else compareTrace(rest1, rest2)
    case _ => false
  }

def traceBits(r: Rexp, trace: List[Trace]): (Bits, List[Trace]) =
  r match {
    case ONE => (Nil, trace)
    case CHAR(_) => (Nil, trace)
    case ALT(r1, r2) =>
      trace match { 
        case Alt(_, true) :: rest =>
          val (bs, rest1) = traceBits(r1, rest)
          (Bit.Z :: bs, rest1)
        case Alt(_, false) :: rest =>
          val (bs, rest1) = traceBits(r2, rest)
          (Bit.S :: bs, rest1) 
      }
    case SEQ(r1, r2) =>
      val (bs2, rest2) = traceBits(r2, trace)
      val (bs1, rest1) = traceBits(r1, rest2)
      (bs1 ::: bs2, rest1)
    
    case STAR(r) =>
      trace match {
        case StarTT(false) :: StarTT(true) :: rest =>
          val (bs1, rest1) = traceBits(r, rest)
          val (bs2, rest2) = traceBits(STAR(r), rest1)
          (bs2.init ::: (Bit.Zz :: bs1) ::: List(Bit.Ss), rest2)

        case StarTT(false) :: rest =>
          (List(Bit.Ss), rest)
      }

    /* case STAR(r) =>
      trace match {
        case StarT(_, false) :: StarT(_, true) :: rest =>
          val (bs1, rest1) = traceBits(r, rest)
          val (bs2, rest2) = traceBits(STAR(r), rest1)
          (bs2.init ::: (Bit.Zz :: bs1) ::: List(Bit.Ss), rest2)

        case StarT(_, false) :: rest =>
          (List(Bit.Ss), rest)
      } */
    case NTIMES(r, n) =>
      if (n == 0) (List(Bit.Ss), trace)
      else {
        val (bs1, rest1) = traceBits(r, trace)
        val (bs2, rest2) = traceBits(NTIMES(r, n - 1), rest1)
        (bs2.init ::: (Bit.Zz :: bs1) ::: List(Bit.Ss), rest2)
      }
}

def decodeTrace(r: Rexp, trace: List[Trace]): (Val, List[Trace]) =
  r match {
    case ONE => (Empty, trace)
    case CHAR(c) => (Chr(c), trace)
    case ALT(r1, r2) =>
      trace match {
        case Alt(_, true) :: rest =>
          val (v, rest1) = decodeTrace(r1, rest)
          (Left(v), rest1)
        case Alt(_, false) :: rest =>
          val (v, rest1) = decodeTrace(r2, rest)
          (Right(v), rest1)
      }
    case SEQ(r1, r2) =>
      val (v2, rest2) = decodeTrace(r2, trace)
      val (v1, rest1) = decodeTrace(r1, rest2)
      (Sequ(v1, v2), rest1)
    
    case STAR(r) =>
      trace match {
        case StarTT(false) :: StarTT(true) :: rest =>
          val (v, rest1) = decodeTrace(r, rest)
          val (Stars(vs), rest2) = decodeTrace(STAR(r), rest1)
          (Stars(vs :+ v), rest2)

        case StarTT(false) :: rest =>
          (Stars(Nil), rest)
      }

    /* case STAR(r) =>
      trace match {
        case StarT(_, false) :: StarT(_, true) :: rest =>
          val (v, rest1) = decodeTrace(r, rest)
          val (Stars(vs), rest2) = decodeTrace(STAR(r), rest1)
          (Stars(vs :+ v), rest2)

        case StarT(_, false) :: rest => (Stars(Nil), rest)
      } */
    case NTIMES(r, n) =>
      if (n == 0) (Nt(Nil, 0), trace)
      else {
        val (v, rest1) = decodeTrace(r, trace)
        val (Nt(vs, _), rest2) = decodeTrace(NTIMES(r, n - 1), rest1)
        (Nt(vs :+ v, n), rest2)
      }
      
}

def matcher3(r: Rexp, s: String): Boolean = {
  val a = mat3(r, s)
  a(s.length) != null && a(s.length)._1
}

def lex3(r: Rexp, s: String): Bits = {
  val a = mat3(r, s)
  traceBits(r, a(s.length)._2)._1
}

def lexer3(r: Rexp, s: String): Val =
  decode(r, lex3(r, s))

def lexer3v(r: Rexp, s: String): Val = {
  val a = mat3(r, s)
  decodeTrace(r, a(s.length)._2)._1
}

def testMat3(): Unit = {
  val examples = List[(Rexp, String)](
    ((ONE | "a") ~ ("aa" | "a"), "aa"),

    ((ONE | "a") ~ ("ab" | "b"), "ab"),
    ((ONE | "c") ~ (("c" ~ "c") | "c"), "cc"),
    ("aa" | ("a" ~ (ONE ~ "a")), "aa"),
    ((ONE ~ "a") | ("a" ~ ONE), "a"),
    ((("a" | "b") | "b"), "b"),
    ("a" | ("ab" | "ba"), "ab"),
    ((("a" | "ab") ~ ("b" | ONE)), "ab"),
    ("abc", "abc"),
    ((("a" | ("a" ~ "a")) ~ ("a" | ("a" ~ "a"))), "aaa"),
    (((("a" ~ "a") | "a") ~ ("a" | ("a" ~ "a"))), "aaa"),
    ((("a" | ("a" ~ "a")) ~ ("a" | ("a" ~ "a"))), "aaa"),
    (((("a" | "c") ~ ("c" ~ "b")) | ((ZERO ~ ONE) ~ ONE)), "acb"),
    (ONE | "a", "a"),

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
    (("b" ~ ONE | %("a")) ~ %("a" | "b"), "aab"),
    (%("b" | %("a")), "bab")
  )

  var inequality = 0

  examples.zipWithIndex.foreach { case ((r, s), i) =>
    val marks = lex3(r, s)
    val der = blex_simp(internalise(r), s.toList).reverse

    if (marks != der) inequality += 1

    println(s"Test ${i + 1}")
    println(s"Regex: $r")
    println(s"Input: $s")
    println(s"Marks: ${marks.mkString("(", ",", ")")}")
    println(s"Derivative: ${der.mkString("(", ",", ")")}")
    println(s"Equal: ${marks == der}")
    println()
  }

  println(s"Inequality: $inequality / ${examples.length}")
}



def mat4(r: Rexp, s: String): Array[(Boolean, List[Trace])] = {
  val tArray = new Array[(Boolean, List[Trace])](s.length + 1)

  def shifts2(ms: marksC, r: Rexp, flag: Boolean = true): marksC =
    r match {
      case ZERO => Nil

      case ONE =>
        if (flag)
          //updates the array if not recorded.??
          for ((m, trace) <- ms)
            tArray(m) match {
              case null => tArray(m) = (true, trace)
              case (_, Nil) => tArray(m) = (true, trace)
              case (_, trace0) if compareTrace(trace, trace0) => tArray(m) = (true, trace)
              case _ =>
            } 
        ms
      case CHAR(c) =>
        for ((m, trace) <- ms if m < s.length && s(m) == c) yield {
          if (flag)
            tArray(m + 1) match {
              case null => tArray(m + 1) = (true, trace)
              case (_, trace0) if compareTrace(trace, trace0) => tArray(m + 1) = (true, trace)
              case _ =>
            }
          (m + 1, trace)
        }

      case ALT(r1, r2) =>
        val ms1 = shifts2(ms, r1, false).map { case (m, trace) => (m, Alt(m, true) :: trace) }
        val ms2 = shifts2(ms, r2, false).map { case (m, trace) => (m, Alt(m, false) :: trace) }
        val ms3 = ms1 ::: ms2
        if (flag)
          for ((m, trace) <- ms3)
            tArray(m) match {
              case null => tArray(m) = (true, trace)
              case (_, Nil) => tArray(m) = (true, trace)
              case (_, trace0) if compareTrace2(trace, trace0) => tArray(m) = (true, trace)
              case _ =>
            }
        ms3

      case SEQ(r1, r2) => shifts2(shifts2(ms, r1, false), r2, flag)
      case STAR(r) =>
        val ms0 = ms.map { case (m, trace) => (m, StarTT(false) :: trace) } //zero repitions, false= 1 in bitcode
        val ms1 = shifts2(ms0, r, false).filterNot { case (m, _) => ms.exists(_._1 == m) }
          .map { case (m, trace) => (m, StarTT(true) :: trace) } // new iteration

        val ms2 = if (ms1.isEmpty) ms0 else ms0 ::: shifts2(ms1, STAR(r), false)

        if (flag)
          for ((m, trace) <- ms2)
            tArray(m) match {
              case null => tArray(m) = (true, trace)
              case (_, Nil) => tArray(m) = (true, trace)
              case (_, trace0) if compareTrace2(trace, trace0) => tArray(m) = (true, trace)
              case _ =>
            }

        ms2

      case NTIMES(r, n) =>
        if (n == 0) shifts2(ms, ONE, flag)
        else shifts2(shifts2(ms, r, false), NTIMES(r, n - 1), flag)
    }

  tArray(0) = (true, Nil)
  val marks=shifts2(List((0, Nil)), r)
  for(m <- tArray) println(m)
  marks.foreach{m => println(m)}
  tArray
}

def compareTrace2(ts1: List[Trace], ts2: List[Trace]): Boolean =
  (ts1, ts2) match {
    case (StarTT(_) :: rest1, _) => compareTrace(rest1, ts2)
    case (_, StarTT(_) :: rest2) => compareTrace(ts1, rest2)

    case (Alt(m1, left1) :: rest1, Alt(m2, left2) :: rest2) =>
      if (m1 != m2) m1 > m2
      else if (left1 != left2) left1
      else compareTrace(rest1, rest2)

    case _ => false
  }

def matcher4(r: Rexp, s: String): Boolean = {
  val a = mat4(r, s)
  a(s.length) != null && a(s.length)._1
}

def lex4(r: Rexp, s: String): Bits = {
  val a = mat4(r, s)
  a(s.length) match {
    case null => throw new Exception("no match")
    case (_, trace) => traceBits(r, trace)._1
  }
}

def lexer4(r: Rexp, s: String): Val =
  decode(r, lex4(r, s))

def lexer4v(r: Rexp, s: String): Val = {
  val a = mat4(r, s)
  a(s.length) match {
    case null => throw new Exception("no match")
    case (_, trace) => decodeTrace(r, trace)._1
  }
}

def testMat4(): Unit = {
  val examples = List[(Rexp, String)](
    ((ONE | "a") ~ ("aa" | "a"), "aa"),

    ((ONE | "a") ~ ("ab" | "b"), "ab"),
    ((ONE | "c") ~ (("c" ~ "c") | "c"), "cc"),
    ("aa" | ("a" ~ (ONE ~ "a")), "aa"),
    ((ONE ~ "a") | ("a" ~ ONE), "a"),
    ((("a" | "b") | "b"), "b"),
    ("a" | ("ab" | "ba"), "ab"),
    ((("a" | "ab") ~ ("b" | ONE)), "ab"),
    ("abc", "abc"),
    ((("a" | ("a" ~ "a")) ~ ("a" | ("a" ~ "a"))), "aaa"),
    (((("a" ~ "a") | "a") ~ ("a" | ("a" ~ "a"))), "aaa"),
    ((("a" | ("a" ~ "a")) ~ ("a" | ("a" ~ "a"))), "aaa"),
    (((("a" | "c") ~ ("c" ~ "b")) | ((ZERO ~ ONE) ~ ONE)), "acb"),
    (ONE | "a", "a"),
    (( ( ZERO |"a" ) | ( "a"|ZERO )) , "a"),

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
    (("b" ~ ONE | %("a")) ~ %("a" | "b"), "aab"),
    (%("b" | %("a")), "bab")
  )

  var inequality = 0

  examples.zipWithIndex.foreach { case ((r, s), i) =>
    val marks = lex4(r, s)
    val der = blex_simp(internalise(r), s.toList).reverse

    if (marks != der) inequality += 1

    println(s"Test ${i + 1}")
    println(s"Regex: $r")
    println(s"Input: $s")
    println(s"Marks: ${marks.mkString("(", ",", ")")}")
    println(s"Derivative: ${der.mkString("(", ",", ")")}")
    println(s"Equal: ${marks == der}")
    println()
  }

  println(s"Inequality: $inequality / ${examples.length}")
}

val r1=(ONE | "a") ~ ("aa" | "a")
val s1="aa"

val r2=("a" | "ab") ~ ("b" | ONE)
val s2="ab"

val r3= (ONE ~ "a") | ("a" ~ ONE)
val s3="a"

val r4=(("a" | ZERO) | (ONE ~ ONE) ) | ( (ONE|"a") ~ (ZERO| ONE) )
val s4="a"

val r5="a" | %("a")
val s5="a"

val r6= %("a") ~ ("aa")
val s6="aa"

val r7= %("b" | %("a"))
val s7="bab"

val r8= %("a") | %("aa")
val s8="aa"

val r9= ( ( ZERO |"a" ) | ( "a"|ZERO )) 
val s9="a"


def fromSeq[A](xs: Seq[A]): PushGen[A] = PushGen(v => xs.foreach(v)) 
  // enumerating Rexps using by default "ab" as alphabet
  def rexps(fuel: Int, alphabet: Seq[Char] = "abc".toSeq): PushGen[Rexp] =
    if fuel <= 0 then PushGen[Rexp](_ => ())            // nothing fits in depth 0
    else
      val sub  = rexps(fuel - 1, alphabet)              // built once, reused below
      val zero = PushGen[Rexp](v => v(ZERO))       // nullary leaves
      val one  = PushGen[Rexp](v => v(ONE))
      val chr  = fromSeq(alphabet).map(CHAR(_))    // leaf carrying a Char
      val alt  = for r1 <- sub; r2 <- sub yield ALT(r1, r2)   // binary
      val seq  = for r1 <- sub; r2 <- sub yield SEQ(r1, r2)   // binary
      val star = sub.map(STAR(_))                  // unary
      zero ++ one ++ chr ++ alt ++ seq ++ star


def testall(maxRegexes: Int): Unit = {
  val alphabet = LazyList('a', 'b', 'c')
  var i = 0
  rexps(4).take(maxRegexes).foreach { r =>
    i += 1
    if (i % 100_000 == 0)print("*")
    for (s <- generate_up_to(alphabet)(10)(r).take(10) if s != "") {
      val vm = lexer4v(r, s)
      val vb = blexer(r, s)
      if (vm != vb) {
        println("Mismatch:")
        println(s"Regex: $r")
        println(s"String: $s")
        println(s"Marks: $vm")
        println(s"Derivative: $vb")
        sys.exit(1)
      }
    }
  }
  println(s"No mismatches found in the first $i regexes.")
}
@main def runTestAll(maxRegexes: Int): Unit =
  testall(maxRegexes)