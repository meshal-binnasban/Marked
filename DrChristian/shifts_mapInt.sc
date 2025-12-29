import scala.collection.immutable.IntMap
import scala.collection.mutable

import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.rexps, rexps._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit

final case class Bits(v: BigInt, len: Int) {
  def appendBit(b: Int): Bits = {
    val newV = (v << 1) | BigInt(b)   // append b to the right
    Bits(newV, len + 1)
  }
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

case class Mark( m: Int, bits: Bits, kStar: Int = -1, starId: Int = -1, kSeq:  Int = -1, seqId:  Int = -1) {
  override def toString: String =
    s"(m=$m, kStar=$kStar, starId=$starId, kSeq=$kSeq, seqId=$seqId, bits=${bitsToList(bits)})"
}

type Marks = IntMap[Mark]

def prefer(old: Mark, neu: Mark): Mark = {
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

def merge(a: Marks, b: Marks): Marks = {
  val acc = mutable.LongMap.empty[Mark]
  a.foreach { case (k, v) => acc.update(k.toLong, v) }
  b.foreach { case (k, neu) =>
    val kk = k.toLong
    acc.get(kk) match {
      case None      => acc.update(kk, neu)
      case Some(old) => acc.update(kk, prefer(old, neu))
    }
  }
  IntMap.from(acc.iterator.map { case (kk, v) => (kk.toInt, v) })
}


extension (ms: Marks)
  def appendBit(b: Int): Marks =
    ms.map { case (m, p) => (m, p.copy(bits = p.bits.appendBit(b))) }

  def appendBits(bs: Bits): Marks =
    if (bs.len == 0) ms
    else ms.map { case (m, p) => (m, p.copy(bits = bitsConcat(p.bits, bs))) }


def mkeps2(r: RexpS): Bits = r match {
  case ONES => Bits.empty
  case ALTS(r1, r2) => if (nullableS(r1)) bitsConcat(Bits.empty.appendBit(0), mkeps2(r1))  // L = 0
    else bitsConcat(Bits.empty.appendBit(1), mkeps2(r2))  // R = 1
  case SEQS(r1, r2, _) => bitsConcat(mkeps2(r1), mkeps2(r2))
  case STARSS(_, _) => Bits.empty.appendBit(1) // En = 1 
  case NTIMESS(_, _) => Bits.empty.appendBit(1) // EnT = 1
  case ANDS(r1, r2) => bitsConcat(mkeps2(r1), mkeps2(r2))
  //case _ => Bits.empty
}

def shifts(ms: Marks, s: String, r: RexpS): Marks = {
  val acc = mutable.LongMap.empty[Mark]
  ms.valuesIterator.foreach { p =>
    val out = shifts1(p, s, r)
    out.foreach { case (k, neu) =>
      val kk = k.toLong
      acc.get(kk) match {
        case None      => acc.update(kk, neu)
        case Some(old) => acc.update(kk, prefer(old, neu))
      }
    }
  }
  IntMap.from(acc.iterator.map { case (kk, v) => (kk.toInt, v) })
}

def shifts1(p: Mark, s: String, r: RexpS): Marks = r match {
  case ZEROS => IntMap.empty
  case ONES  => IntMap.empty
  case CHARS(c) =>
    if (p.m < s.length && s(p.m) == c) {
      val q = p.copy(m = p.m + 1)
      IntMap(q.m -> q)
    } else IntMap.empty

  case ALTS(r1, r2) =>
    val ms0   = IntMap(p.m -> p)
    val left  = shifts(ms0.appendBit(0), s, r1) // L = 0
    val right = shifts(ms0.appendBit(1), s, r2) // R = 1
    merge(left, right) 

  case SEQS(r1, r2, id) =>
    val ms0 = IntMap(p.m -> p)
    val ms1 = shifts(ms0, s, r1)

    val n1 = nullableS(r1)
    val n2 = nullableS(r2)

    val inR20 = if (n1) merge(ms1, ms0.appendBits(mkeps2(r1))) else ms1
    val inR2 = inR20.map { case (m, q) => if (q.kSeq == -1) (m, q.copy(kSeq = q.m, seqId = id)) else (m, q)}

    val r1Consume0 = if (n2) ms1.appendBits(mkeps2(r2)) else IntMap.empty
    val r1Consume = r1Consume0.map { case (m, q) => if (q.kSeq == -1) (m, q.copy(kSeq = q.m, seqId = id)) else (m, q)}

    val r1r2Consume = shifts(inR2, s, r2)
    if (n2) merge(r1Consume, r1r2Consume) else r1r2Consume

  case STARSS(r, id) =>
    val ms0    = IntMap(p.m -> p)
    val ms10 = shifts(ms0.appendBit(0), s, r) // Nx = 0

    val ms1 = ms10.map { case (m, q) => if (q.kStar == -1) (m, q.copy(kStar = q.m, starId = id)) else (m, q) }

    if (ms1.isEmpty) IntMap.empty
    else {
      val outEn  = ms1.appendBit(1) // En = 1
      val outRec = shifts(ms1, s, STARSS(r, id))
      merge(outEn, outRec)
    }

  case NTIMESS(r, n) =>
    val ms0 = IntMap(p.m -> p)
    if (n == 0) IntMap.empty
    else if (n == 1) shifts(ms0.appendBit(0), s, r).appendBit(1) // NxT=0 EnT=1
    else {
      val ms1 = shifts(ms0.appendBit(0), s, r)
      if (ms1.isEmpty) IntMap.empty
      else if (nullableS(r)) merge(ms1.appendBit(1), shifts(ms1, s, NTIMESS(r, n - 1)))
      else shifts(ms1, s, NTIMESS(r, n - 1))
    }

  case ANDS(r1, r2) =>
    val ms0 = IntMap(p.m -> p)
    val l   = shifts(ms0, s, r1)
    val rr  = shifts(ms0, s, r2)
    l.keySet.intersect(rr.keySet).foldLeft(IntMap.empty[Mark]) { (acc, k) =>
      acc.updated(k, l(k))
    }
}

def mat(r: Rexp, s: String): Marks =
  shifts(IntMap(0 -> Mark(0, Bits.empty)), s, intern(r))

def matcher(r: Rexp, s: String): Boolean =
  if (s == "") nullable(r) else mat(r, s).contains(s.length)

def lex(r: Rexp, s: String): List[Bit] =
  if (s == "") {
    if (nullable(r)) bitsToBits(intern(r), mkeps2(intern(r))) else Nil
  } else {
    mat(r, s).get(s.length) match {
      case Some(mark) => bitsToBits(intern(r), mark.bits)
      case None       => Nil
    }
  }

def lexer(r: Rexp, s: String): Val =
  if (s == "") {
    if (nullable(r)) decode(intern(r), mkeps2(intern(r))) else Invalid
  } else {
    mat(r, s).get(s.length) match {
      case Some(mark) => decode(intern(r), mark.bits)
      case None       => Invalid
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

  case (ONES, bs) =>
    (Empty, bs)

  case (CHARS(c), bs) =>
    (Chr(c), bs)

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
    val (Stars(vs), bs3) =
      (decode_aux(STARSS(r1, id), bs2) : @unchecked)
    (Stars(v :: vs), bs3)

  case (STARSS(_, _), 1 :: bs1) =>
    (Stars(Nil), bs1)

  case (NTIMESS(r1, n), 0 :: bs1) =>
    val (v, bs2) = decode_aux(r1, bs1)
    val (Nt(vs, _), bs3) =
      (decode_aux(NTIMESS(r1, n - 1), bs2) : @unchecked)
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
  val reg9 = (( "a" ~ ONE ) | ( ONE ~ "a" )) ~ %("a") 
  val s9   = "a"
  println(s"9- $s9: r=\n${pp(reg9)}  ${mat(reg9, s9)} ")
  println(lexer(reg9, s9))
  println(rebit.blexer(reg9, s9))
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
    val r = enumerate.decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- generate_up_to(alphabet)(20)(r).take(19) if s != "") {
      val vm  = lexer(r, s)
      val vb  = rebit.blexer(r, s)
      val res = vm == vb
      if (!res) {
        println(s"$r and $s")
        println(s"Marks=$vm derivative=$vb")
        System.exit(1)
      }
    }
  }
}