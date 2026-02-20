import scala.collection.mutable
import scala.collection.immutable.IntMap
import scala.language.implicitConversions

import $file.rexp, rexp._
import $file.rexps, rexps._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit

// ------------------------------ Bits (BigInt packed) ------------------------------

final case class Bits(v: BigInt, len: Int) {
  def appendBit(b: Int): Bits = Bits((v << 1) | BigInt(b), len + 1)
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
  while (k > 0) { out = (v & 1).toInt :: out; v = v >> 1; k -= 1 }
  out
}

// ------------------------------ Global info map + prefer ------------------------------

case class MarkInfo(
  bits: Bits = Bits.empty,
  kStar: Int = -1, starId: Int = -1,
  kSeq:  Int = -1, seqId:  Int = -1,
  seqMode: Int = 0,
  firstConsumed: Option[Int] = None
)

val infoMap: mutable.LongMap[MarkInfo] = mutable.LongMap.empty[MarkInfo]

def prefer(old: MarkInfo, neu: MarkInfo): MarkInfo = {
  val sameStar = old.starId >= 0 && old.starId == neu.starId
  if (sameStar) {
    if (neu.kStar > old.kStar) neu
    else if (neu.kStar < old.kStar) old
    else {
      val sameSeq = old.seqId >= 0 && old.seqId == neu.seqId
      if (sameSeq && neu.kSeq > old.kSeq) neu else old
    }
  } else {
    val sameSeq = old.seqId >= 0 && old.seqId == neu.seqId
    if (sameSeq && neu.kSeq > old.kSeq) neu else old
  }
}

def updateInfo(m: Int, mi: MarkInfo): Unit = {
  val k = m.toLong
  infoMap.get(k) match {
    case None      => infoMap.update(k, mi)
    case Some(old) => infoMap.update(k, prefer(old, mi))
  }
}

// ------------------------------ mkeps2 (nullable-only bits) ------------------------------

def mkeps2(r: RexpS): Bits = r match {
  case ONES => Bits.empty
  case ALTS(r1, r2, _) =>
    if (nullableS(r1)) bitsConcat(Bits.empty.appendBit(0), mkeps2(r1))
    else              bitsConcat(Bits.empty.appendBit(1), mkeps2(r2))
  case SEQS(r1, r2, _) => bitsConcat(mkeps2(r1), mkeps2(r2))
  case STARSS(_, _)    => Bits.empty.appendBit(1) // En = 1
  case NTIMESS(_, _)   => Bits.empty.appendBit(1) // EnT = 1
  case ANDS(r1, r2)    => bitsConcat(mkeps2(r1), mkeps2(r2))
  case ZEROS           => throw new Exception("mkeps2: ZEROS is not nullable")
  case CHARS(_)        => throw new Exception("mkeps2: CHARS(_) is not nullable")
  // If your CHARS is CHARS(c,id) use: case CHARS(_, _) => ...
}

// ------------------------------ shifts (Set[Int] marks) ------------------------------

type Marks = Set[Int]

def shiftsWith(ms: Set[Int], mi: MarkInfo, s: String, r: RexpS, local: scala.collection.mutable.LongMap[MarkInfo]): Set[Int] =
  ms.flatMap { m =>
    val miM = local.getOrElse(m.toLong, infoMap.getOrElse(m.toLong, mi))
    shifts1(m, miM, s, r)  // shifts1 stays unchanged
  }

def shifts(ms: Marks, mi: MarkInfo, s: String, r: RexpS): Marks =
  ms.flatMap(m => shifts1(m, mi, s, r))

def shifts1(m: Int, mi: MarkInfo, s: String, r: RexpS): Marks = r match {
  case ZEROS => Set.empty
  case ONES  => Set.empty

  // If your CHARS has an id (CHARS(c,id)), change this to: case CHARS(c, _) =>
  case CHARS(c) =>
    if (m < s.length && s(m) == c) {
      val fc = mi.firstConsumed.orElse(Some(m))
      updateInfo(m + 1, mi.copy(firstConsumed = fc))
      Set(m + 1)
    } else Set.empty

  case ALTS(r1, r2, _) =>
    shifts(Set(m), mi.copy(bits = mi.bits.appendBit(0)), s, r1) ++
    shifts(Set(m), mi.copy(bits = mi.bits.appendBit(1)), s, r2)

  case SEQS(r1, r2, id) =>
    val n1 = nullableS(r1)
    val n2 = nullableS(r2)

    val ms1 = shifts(Set(m), mi.copy(seqId = id), s, r1)

    val famA: Set[Int] =
      if (n2) {
        ms1.map { k =>
          val miK     = infoMap.getOrElse(k.toLong, mi.copy(seqId = id))
          val epsBits = bitsConcat(miK.bits, mkeps2(r2))                 // <<< FIX: use miK.bits
          updateInfo(k, miK.copy(seqId = id, kSeq = k, seqMode = 0, bits = epsBits))
          k
        }
      } else Set.empty

    val famB: Set[Int] =
      ms1.flatMap { k =>
        val miK = infoMap.getOrElse(k.toLong, mi.copy(seqId = id))
        shifts(Set(k), miK.copy(seqId = id, kSeq = k, seqMode = 1), s, r2)
      }

    val famC: Set[Int] =
      if (n1) {
        val epsBits = bitsConcat(mi.bits, mkeps2(r1))                    // here: prefix is mi at SEQ entry (correct)
        shifts(Set(m), mi.copy(seqId = id, seqMode = 0, bits = epsBits), s, r2)
      } else Set.empty

    famA ++ famB ++ famC


  case STARSS(r1, id) =>
    // (A) one iteration: Nx = 0
    val ms1 = shifts(Set(m), mi.copy(starId = id, bits = mi.bits.appendBit(0)), s, r1)
    if (ms1.isEmpty) Set.empty
    else {
      // Snapshot the *continuation* infos for ms1 BEFORE writing any stop-candidates (En)
      val cont = scala.collection.mutable.LongMap.empty[MarkInfo]
      ms1.foreach { k =>
        val miK = infoMap.getOrElse(k.toLong, mi.copy(starId = id))
        cont.update(k.toLong, miK.copy(starId = id)) // crucially: WITHOUT En
      }

      // (B) stopping candidates: append En = 1 and commit them
      val outEn: Set[Int] = ms1.map { k =>
        val miK = cont(k.toLong)
        val stop = miK.copy(starId = id, kStar = k, bits = miK.bits.appendBit(1))
        updateInfo(k, stop)
        k
      }

      // (C) continue recursion using the continuation snapshot (so it never sees En-in-the-middle)
      val outRec = shiftsWith(ms1, mi.copy(starId = id), s, STARSS(r1, id), cont)

      outEn ++ outRec
  }



  case NTIMESS(r1, n) =>
    if (n == 0) Set.empty
    else if (n == 1) shifts(Set(m), mi.copy(bits = mi.bits.appendBit(0)), s, r1)
    else {
      val ms1 = shifts(Set(m), mi.copy(bits = mi.bits.appendBit(0)), s, r1)
      if (ms1.isEmpty) Set.empty
      else if (nullableS(r1)) ms1 ++ shifts(ms1, mi.copy(bits = mi.bits.appendBit(1)), s, NTIMESS(r1, n - 1))
      else shifts(ms1, mi, s, NTIMESS(r1, n - 1))
    }

  case ANDS(r1, r2) =>
    shifts(Set(m), mi, s, r1) intersect shifts(Set(m), mi, s, r2)
}

// ------------------------------ mat / matcher / lex / lexer ------------------------------

def mat(r: Rexp, s: String): Marks = {
  infoMap.clear()
  val rs = intern(r)
  val out = shifts(Set(0), MarkInfo(bits = Bits.empty), s, rs)
  out
}

def matcher(r: Rexp, s: String): Boolean =
  if (s == "") nullable(r) else mat(r, s).contains(s.length)

def lex(r: Rexp, s: String): List[Bit] =
  if (s == "") {
    if (nullable(r)) bitsToBits(intern(r), mkeps2(intern(r))) else Nil
  } else {
    val out = mat(r, s)
    println(s"infoMap=$infoMap")
    infoMap.get(s.length.toLong) match {
      case Some(mi) => bitsToBits(intern(r), mi.bits)
      case None     => Nil
    }
  }

def lexer(r: Rexp, s: String): Val =
  if (s == "") {
    if (nullable(r)) decode(intern(r), mkeps2(intern(r))) else Invalid
  } else {
    val out = mat(r, s)
    println(s"infoMap=$infoMap")
    infoMap.get(s.length.toLong) match {
      case Some(mi) => decode(intern(r), mi.bits)
      case None     => Invalid
    }
  }

// ------------------------------ bitsToBits / decode ------------------------------

def bitsToBits(r: RexpS, b: Bits): List[Bit] = {
  val it = bitsToList(b).iterator

  def go(re: RexpS): List[Bit] = re match {
    case ZEROS | ONES | CHARS(_) => Nil
    // If CHARS has id: case ZEROS | ONES | CHARS(_, _) => Nil

    case ANDS(r1, r2)      => go(r1) ++ go(r2)
    case SEQS(r1, r2, _)   => go(r1) ++ go(r2)

    case ALTS(r1, r2, _) =>
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
  // If CHARS has id: case (CHARS(c, _), bs) => (Chr(c), bs)

  case (ALTS(r1, r2, _), 0 :: bs1) =>
    val (v, bs2) = decode_aux(r1, bs1); (Left(v), bs2)

  case (ALTS(r1, r2, _), 1 :: bs1) =>
    val (v, bs2) = decode_aux(r2, bs1); (Right(v), bs2)

  case (SEQS(r1, r2, _), bs0) =>
    val (v1, bs1) = decode_aux(r1, bs0)
    val (v2, bs2) = decode_aux(r2, bs1)
    (Sequ(v1, v2), bs2)

  case (STARSS(r1, id), 0 :: bs1) =>
    val (v, bs2) = decode_aux(r1, bs1)
    val (Stars(vs), bs3) = (decode_aux(STARSS(r1, id), bs2) : @unchecked)
    (Stars(v :: vs), bs3)

  case (STARSS(_, _), 1 :: bs1) =>
    (Stars(Nil), bs1)

  case (NTIMESS(r1, n), 0 :: bs1) =>
    val (v, bs2) = decode_aux(r1, bs1)
    val (Nt(vs, _), bs3) = (decode_aux(NTIMESS(r1, n - 1), bs2) : @unchecked)
    (Nt(v :: vs, n), bs3)

  case (NTIMESS(_, _), 1 :: bs1) =>
    (Nt(Nil, 0), bs1)

  case (ANDS(r1, r2), bs0) =>
    val (v1, bs1) = decode_aux(r1, bs0)
    val (v2, bs2) = decode_aux(r2, bs1)
    (Sequ(v1, v2), bs2)

  case (ZEROS, _) =>
    throw new Exception("Not decodable: ZEROS reached")

  case _ =>
    throw new Exception("Not decodable: bitstream does not match regex structure")
}

def decode(r: RexpS, packed: Bits): Val = {
  val bs = bitsToList(packed)
  decode_aux(r, bs) match {
    case (v, Nil) => v
    case _        => throw new Exception("Not decodable: leftover bits")
  }
}

// ------------------------------ Tests (as you provided) ------------------------------

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
  println(lex(reg9,s9))
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
