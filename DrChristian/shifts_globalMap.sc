import scala.collection.mutable
import scala.language.implicitConversions

import $file.rexp, rexp._
import $file.rexps, rexps._
import $file.rebit

// ======================================================
// Bits - use of BigInt instead of BitList
// ======================================================
final case class Bits(v: BigInt, len: Int) {
  def appendBit(b: Int): Bits =
    Bits((v << 1) | BigInt(b), len + 1)
}
object Bits {
  val empty: Bits = Bits(BigInt(0), 0)
}

def bitsConcat(a: Bits, b: Bits): Bits =
  if (b.len == 0) a else Bits((a.v << b.len) | b.v, a.len + b.len)

def bitsToList(b: Bits): List[Int] = {
  var v = b.v
  var k = b.len
  var out: List[Int] = Nil
  while (k > 0) { out = (v & 1).toInt :: out; v >>= 1; k -= 1 }
  out
}

// ======================================================
// Mark info 
// ======================================================


case class Info(
  bits: Bits,
  kStar: Int = -1,
  starId: Int = -1,
  kSeq:  Int = -1,
  seqId: Int = -1,
  seqMode: Int = -1
)

val infoMap = mutable.LongMap.empty[Info]

def prefer(old: Info, neu: Info): Info = {
  val sameStar = old.starId >= 0 && old.starId == neu.starId
  if (sameStar) {
    if (neu.kStar > old.kStar) return neu
    if (neu.kStar < old.kStar) return old
  }
  val sameSeq = old.seqId >= 0 && old.seqId == neu.seqId
  if (sameSeq) {
    if (neu.seqMode < old.seqMode) return neu
    if (neu.seqMode > old.seqMode) return old
    if (neu.kSeq > old.kSeq) return neu
    if (neu.kSeq < old.kSeq) return old
  }

  old
}

def updateInfo(m: Int, info: Info): Unit = {
  val k = m.toLong
  infoMap.get(k) match {
    case None => infoMap.update(k, info)
    case Some(old) => infoMap.update(k, prefer(old, info))
  }
}
// ======================================================
// mkeps2 
// ======================================================

def mkeps2(r: RexpS): Bits = r match {
  case ONES => Bits.empty
  case ALTS(r1, r2) => if (nullableS(r1)) bitsConcat(Bits.empty.appendBit(0), mkeps2(r1))
    else bitsConcat(Bits.empty.appendBit(1), mkeps2(r2))
  case SEQS(r1, r2, _) => bitsConcat(mkeps2(r1), mkeps2(r2))
  case STARSS(_, _) => Bits.empty.appendBit(1)   // En
  case NTIMESS(_, _) => Bits.empty.appendBit(1)
  case ANDS(r1, r2) => bitsConcat(mkeps2(r1), mkeps2(r2))
}

// ======================================================
// shifts
// ======================================================

def shifts(ms: Set[Int], bits: Bits, s: String, r: RexpS): Set[Int] =
  ms.flatMap(m => shifts1(m, bits, s, r))

def shifts1(m: Int, bits: Bits, s: String, r: RexpS): Set[Int] = r match {
  // --------------------------------------------------
  case ZEROS =>Set.empty
  case ONES =>Set.empty
  // --------------------------------------------------
  case CHARS(c) =>
    if (m < s.length && s(m) == c) {
        updateInfo(m + 1, Info(bits))
        Set(m + 1)
    } else Set.empty
  // --------------------------------------------------
  case ALTS(r1, r2) =>
    shifts(Set(m), bits.appendBit(0), s, r1) ++
    shifts(Set(m), bits.appendBit(1), s, r2)
  // --------------------------------------------------
  case SEQS(r1, r2, id) =>
    val n1 = nullableS(r1)
    val n2 = nullableS(r2)
    infoMap.update(m, Info(bits = bits, seqId = id, kSeq = m))
    var out: Set[Int] = Set.empty
    val ms1: Set[Int] = shifts(Set(m), bits, s, r1)
   
    // -------- r1 ; r2 (r2 actually consumes) --------
    ms1.foreach { k =>
        val ms2 = shifts(Set(k), bits, s, r2)

        ms2.foreach { j =>
        if (j > k) {   // real consumption
            updateInfo(j,Info(bits = bits, seqId = id, kSeq = k, seqMode = 1))
            out += j
        }
        }
    }

    // -------- r1 + ε(r2) --------
    if (n2) {
        ms1.foreach { k =>
        val epsBits = bitsConcat(bits, mkeps2(r2))
        updateInfo(
            k,
            Info(bits = epsBits, seqId = id,kSeq = k, seqMode = 0)
        )
        out += k
        }
    }

    // -------- ε(r1) ; r2 --------
    if (n1) {
        val epsBits = bitsConcat(bits, mkeps2(r1))
        val ms2 = shifts(Set(m), epsBits, s, r2)

        ms2.foreach { j =>
        updateInfo(
            j,
            Info(bits = epsBits, seqId = id, kSeq    = m, seqMode = if (j > m) 1 else 0)
        )
        out += j
        }
    }

    out
  // --------------------------------------------------
  case STARSS(r1, id) =>
    // take one iteration
    val iter = shifts(Set(m), bits.appendBit(0), s, r1)
    if (iter.isEmpty) {
      // end immediately
      Set(m)
    } else {
      var out: Set[Int] = Set.empty
      iter.foreach { k =>
        val info0 = infoMap.getOrElse(k.toLong, Info(bits))
        val info1 =
          if (info0.kStar == -1) info0.copy(kStar = k, starId = id)
          else info0
        updateInfo(k, info1)
        out += k
      }
      out ++ shifts(out, bits, s, STARSS(r1, id))
    }
  // --------------------------------------------------
  case NTIMESS(r1, n) =>
    if (n == 0) Set(m)
    else {
      val iter = shifts(Set(m), bits.appendBit(0), s, r1)
      if (nullableS(r1))
        iter ++ shifts(iter, bits.appendBit(1), s, NTIMESS(r1, n - 1))
      else
        shifts(iter, bits, s, NTIMESS(r1, n - 1))
    }
  // --------------------------------------------------
  case ANDS(r1, r2) =>
    shifts(Set(m), bits, s, r1) intersect
    shifts(Set(m), bits, s, r2)
}

// ======================================================
// Interface
// ======================================================

def mat(r: Rexp, s: String): Set[Int] = {
  infoMap.clear()
  shifts(Set(0), Bits.empty, s, intern(r))
}

def matcher(r: Rexp, s: String): Boolean =
  if (s == "") nullable(r) else mat(r, s).contains(s.length)

def lex(r: Rexp, s: String): List[Bit] =
  if (s == "") {
    if (nullable(r)) bitsToBits(intern(r), mkeps2(intern(r))) else Nil
  } else {
    infoMap.clear()
    shifts(Set(0), Bits.empty, s, intern(r))
    infoMap.get(s.length.toLong) match {
      case Some(info) =>
        println(s"lex infoMap=$infoMap")
        bitsToBits(intern(r), info.bits)
      case None =>
        Nil
    }
  }

def lexer(r: Rexp, s: String): Val =
  if (s == "") {
    if (nullable(r)) decode(intern(r), mkeps2(intern(r))) else Invalid
  } else {
    infoMap.clear()
    shifts(Set(0), Bits.empty, s, intern(r))
    infoMap.get(s.length.toLong) match {
      case Some(info) =>
        decode(intern(r), info.bits)
      case None => Invalid
    }
  }

def bitsToBits(r: RexpS, b: Bits): List[Bit] = {
  val it = bitsToList(b).iterator

  def go(re: RexpS): List[Bit] = re match {
    case ZEROS | ONES | CHARS(_) =>
      Nil

    case ANDS(r1, r2) =>
      go(r1) ++ go(r2)

    case SEQS(r1, r2, _) =>
      go(r1) ++ go(r2)

    case ALTS(r1, r2) =>
      if (!it.hasNext) Nil
      else if (it.next() == 0) Lf :: go(r1) else Ri :: go(r2)

    case STARSS(r1, _) =>
      if (!it.hasNext) Nil
      else if (it.next() == 0) Nx :: go(r1) else En :: Nil

    case NTIMESS(r1, _) =>
      if (!it.hasNext) Nil
      else if (it.next() == 0) NxT :: go(r1) else EnT :: Nil
  }

  go(r)
}

def decode_aux(r: RexpS, bs: List[Int]): (Val, List[Int]) = (r, bs) match {
  case (ONES, bs) => (Empty, bs)
  case (CHARS(c), bs) => (Chr(c), bs)
  case (ALTS(r1, r2), 0 :: bs1) =>
    val (v, bs2) = decode_aux(r1, bs1)
    (Left(v), bs2)
  case (ALTS(r1, r2), 1 :: bs1) =>
    val (v, bs2) = decode_aux(r2, bs1)
    (Right(v), bs2)

  case (SEQS(r1, r2, _), bs0) =>
    val (v1, bs1) = decode_aux(r1, bs0)
    val (v2, bs2) = decode_aux(r2, bs1)
    (Sequ(v1, v2), bs2)

  case (STARSS(r1, id), 0 :: bs1) =>
    val (v, bs2) = decode_aux(r1, bs1)
    val (Stars(vs), bs3) = (decode_aux(STARSS(r1, id), bs2) : @unchecked)
    (Stars(v :: vs), bs3)
  case (STARSS(_, _), 1 :: bs1) => (Stars(Nil), bs1)

  case (NTIMESS(r1, n), 0 :: bs1) =>
    val (v, bs2) = decode_aux(r1, bs1)
    val (Nt(vs, _), bs3) = (decode_aux(NTIMESS(r1, n - 1), bs2) : @unchecked)
    (Nt(v :: vs, n), bs3)
  case (NTIMESS(_, _), 1 :: bs1) => (Nt(Nil, 0), bs1)

  case (ANDS(r1, r2), bs0) =>
    val (v1, bs1) = decode_aux(r1, bs0)
    val (v2, bs2) = decode_aux(r2, bs1)
    (Sequ(v1, v2), bs2)

  case (ZEROS, _) => throw new Exception("Not decodable: ZEROS reached")
  case _ => throw new Exception("Not decodable: bitstream does not match regex structure")
}

def decode(r: RexpS, packed: Bits): Val = {
  val bs = bitsToList(packed)
  decode_aux(r, bs) match {
    case (v, Nil) => v
    case _        => throw new Exception("Not decodable: leftover bits")
  }
}


@main
def tests() = {
  val reg = ("aa") | ("a" ~ (ONE ~ "a"))
  val s   = "aa"
  println(s"1- $s: r=\n${pp(reg)}  ${mat(reg, s)} ")
  println(lexer(reg, s))
  println(rebit.blexer(reg, s))
  println("-"*40)

  val reg2 = %("a"|"aa")
  val s2   = "aaa"
  println(s"2- $s2: r=\n${pp(reg2)}  ${mat(reg2, s2)} ")
  println(lexer(reg2, s2))
  println(rebit.blexer(reg2, s2))
  println("-"*40)

  val reg3 = (ONE | %("a"))
  val s3   = "a"
  println(s"3- $s3: r=\n${pp(reg3)}  ${mat(reg3, s3)} ")
  println(lexer(reg3, s3))
  println(rebit.blexer(reg3, s3))
  println("-"*40)

  val reg4 = ("a"| %("a") )
  val s4   = "a"
  println(s"4- $s4: r=\n${pp(reg4)}  ${mat(reg4, s4)} ")
  println(lexer(reg4, s4))
  println(rebit.blexer(reg4, s4))
  println("-"*40)

  val reg5 = ( %(ONE) ~ ("a") )
  val s5   = "a"
  println(s"5- $s5: r=\n${pp(reg5)}  ${mat(reg5, s5)} ")
  println(lexer(reg5, s5))
  println(rebit.blexer(reg5, s5))
  println("-"*40)

  val reg6 = ( %("a") | %("aa") )
  val s6   = "aa"
  println(s"6- $s6: r=\n${pp(reg6)}  ${mat(reg6, s6)} ")
  println(lexer(reg6, s6))
  println(rebit.blexer(reg6, s6))
  println("-"*40)

  val reg7 = ( ( ONE | "a" ) ~ %("a") )
  val s7   = "a"
  println(s"7- $s7: r=\n${pp(reg7)}  ${mat(reg7, s7)} ")
  println(lexer(reg7, s7))
  println(rebit.blexer(reg7, s7))
  println("-"*40)

  val reg8 = ( ONE ~ "a" ) | ("a" ~ ONE ) 
  val s8   = "a"
  println(s"8- $s8: r=\n${pp(reg8)}  ${mat(reg8, s8)} ")
  println(lexer(reg8, s8))
  println(rebit.blexer(reg8, s8))
  println("-"*40)

  val reg9 = (( "a" ~ ONE ) | ( ONE ~ "a" )) ~ %("a") 
  val s9   = "a"
  println(s"9- $s9: r=\n${pp(reg9)}  ${mat(reg9, s9)} ")
  println(lexer(reg9, s9))
  println(rebit.blexer(reg9, s9))
  println("-"*40)
}
@main
def test1() = {
  val reg9 = ("a" ) ~ (ONE|ONE)
  val s9   = "a"
  println(s"9- $s9: r=\n${pp(reg9)}  ${mat(reg9, s9)} ")
  println(lex(reg9,s9))
  println(rebit.lex(reg9, s9.toList))
  println(lexer(reg9, s9))
  println(rebit.blexer(reg9, s9))
  println("-"*40)

}