import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.rexps, rexps._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit

/* case class Mark(n: Int, m: Int, bits: Bits, from: Int, to: Int, k: Int = -1) {
  override def toString: String = {
    val bitsStr = bits.mkString("[", ",", "]")
    s"(n=$n, m=$m, from=$from, to=$to, k=$k, bits=$bitsStr)"
  }
} */

/* case class Mark(n: Int, m: Int, bits: Bits, from: Int, to: Int, k: Int = -1, kSeq: Int = -1) {
  override def toString: String =
    s"(n=$n, m=$m, from=$from, to=$to, k=$k, kSeq=$kSeq, bits=${bits.mkString("[", ",", "]")})"
} */

case class Mark(n: Int, m: Int, bits: Bits, from: Int, to: Int, kStar: Int = -1, starId: Int = -1, kSeq:  Int = -1, seqId:  Int = -1) {
  override def toString: String =
    s"(n=$n, m=$m, from=$from, to=$to, kStar=$kStar, starId=$starId, kSeq=$kSeq, seqId=$seqId, bits=${bits.mkString("[", ",", "]")})"
}

type Key   = (Int, Int)
type Marks = Map[Key, Mark]

def key(p: Mark): Key = (p.n, p.m)

def putFirst(acc: Marks, q: Mark): Marks = {
  val kk = key(q)
  if (acc.contains(kk)) acc else acc.updated(kk, q)
}

/* // Prefer larger k; if k ties, prefer earlier from; otherwise keep old.
def putPrefer(acc: Marks, q: Mark): Marks = {
  val kk = key(q)
  acc.get(kk) match {
    case None => acc.updated(kk, q)
    case Some(old) =>
      if (q.k > old.k) acc.updated(kk, q)
      else if (q.k < old.k) acc
      else if (q.from < old.from) acc.updated(kk, q)
      else acc
  }
} */
// Prefer larger k; if k ties, prefer earlier from; otherwise keep old.
//but only compare if both have k set (>=0)
//prefer left if equal
def putPrefer2(acc: Marks, q: Mark): Marks = {
  val kk = key(q)
  acc.get(kk) match {
    case None => acc.updated(kk, q)
    case Some(old) =>
      val sameStar = old.starId >= 0 && old.starId == q.starId
      if (sameStar) {
        if (q.kStar > old.kStar) acc.updated(kk, q)
        else if (q.kStar < old.kStar) acc
        else {
          val sameSeq = old.seqId >= 0 && old.seqId == q.seqId
          if (sameSeq && q.kSeq > old.kSeq) acc.updated(kk, q) else acc
        }
      } else {
        val sameSeq = old.seqId >= 0 && old.seqId == q.seqId
        if (sameSeq) {
          if (q.kSeq > old.kSeq) acc.updated(kk, q) else acc
        } else {
          acc 
        }
      }
  }
}

def merge(a: Marks, b: Marks): Marks =
  b.valuesIterator.foldLeft(a)(putPrefer2)

extension (ms: Marks)
  def prefixBit(b: Bit): Marks =
    ms.view.mapValues(p => p.copy(bits = p.bits :+ b)).toMap
  def appendBits(bs: Bits): Marks =
    if (bs.isEmpty) ms else ms.view.mapValues(p => p.copy(bits = p.bits ++ bs)).toMap

def shifts(ms: Marks, s: String, r: RexpS): Marks =
  ms.valuesIterator.foldLeft(Map.empty[Key, Mark]) { (acc, p) =>
    merge(acc, shifts1(p, s, r))
  }

def shifts1(p: Mark, s: String, r: RexpS): Marks = {
  val m0 = p.m

  r match {
    case ZEROS => Map.empty
    case ONES  => Map.empty

    case CHARS(c) =>
        if (p.m < s.length && s(p.m) == c) { 
            val q = Mark(n = p.n, m = p.m + 1, bits = p.bits, from = m0, to = p.m + 1, kStar = p.kStar, starId = p.starId, kSeq = p.kSeq, seqId = p.seqId)
            Map(key(q) -> q)
            } else Map.empty

    case ALTS(r1, r2) =>
        val ms0   = Map(key(p) -> p)
        val left  = shifts(ms0.prefixBit(Lf), s, r1)
        val right = shifts(ms0.prefixBit(Ri), s, r2)
        merge(left, right).view.mapValues(q => q.copy(from = m0, to = q.m)).toMap

    case SEQS(r1, r2, id) =>
        val ms0 = Map(key(p) -> p)
        val ms1 = shifts(ms0, s, r1)
        val n1 = nullableS(r1)
        val n2 = nullableS(r2)
        val inR20 = if (n1) merge(ms1, ms0.appendBits(mkeps2(r1))) else ms1
        val inR2 = inR20.view.mapValues { q => if (q.kSeq == -1) q.copy(kSeq = q.m, seqId = id) else q}.toMap
        val r1Consume0 = if (n2) ms1.appendBits(mkeps2(r2)) else Map.empty
        val r1Consume = r1Consume0.view.mapValues { q => if (q.kSeq == -1) q.copy(kSeq = q.m, seqId = id) else q}.toMap
        val r1r2Consume = shifts(inR2, s, r2)
        val out = if (n2) merge(r1Consume, r1r2Consume) else r1r2Consume
        out.view.mapValues(q => q.copy(from = m0, to = q.m)).toMap

    case STARSS(r, id) =>
        val ms0    = Map(key(p) -> p)
        val ms1raw = shifts(ms0.prefixBit(Nx), s, r)
        val ms1 = ms1raw.view.mapValues { q => if (q.kStar == -1) q.copy(kStar = q.m, starId = id) else q}.toMap
        val out = 
            if (ms1.isEmpty) Map.empty
            else {
                val outEn  = ms1.prefixBit(En)
                val outRec = shifts(ms1, s, STARSS(r, id))
                merge(outEn, outRec)
                }

        out.view.mapValues(q => q.copy(from = m0, to = q.m)).toMap

    case NTIMESS(r0, n) =>
      val ms0 = Map(key(p) -> p)
      val out = if (n == 0) Map.empty
        else if (n == 1) {
          val ms1 = shifts(ms0.prefixBit(NxT), s, r0)
          ms1.prefixBit(EnT)
        } else {
          val ms1 = shifts(ms0.prefixBit(NxT), s, r0)
          if (ms1.isEmpty) Map.empty
          else if (nullableS(r0)) {
            val outEn  = ms1.prefixBit(EnT)
            val outRec = shifts(ms1, s, NTIMESS(r0, n - 1))
            merge(outEn, outRec)
          } else {
            shifts(ms1, s, NTIMESS(r0, n - 1))
          }
        }
      out.view.mapValues(q => q.copy(from = m0, to = q.m)).toMap

    case ANDS(r1, r2) =>
      val ms0 = Map(key(p) -> p)
      val l   = shifts(ms0, s, r1)
      val rr  = shifts(ms0, s, r2)
      l.keySet.intersect(rr.keySet).map(k => k -> l(k)).toMap
        .view.mapValues(q => q.copy(from = m0, to = q.m)).toMap
  }
}

def mat(r: Rexp, s: String): Marks =
  shifts( Map((0, 0) -> Mark(0, 0, Nil, 0, 0, -1, -1, -1, -1)), s, intern(r))

def matcher(r: Rexp, s: String): Boolean =
  if (s == "") nullable(r)
  else mat(r, s).contains((0, s.length))

def lexer(r: Rexp, s: String): Bits =
  if (s == "") {
    if (nullable(r)) mkeps2(intern(r)) else Nil
  } else {
    mat(r, s).get((0, s.length)) match {
      case Some(mark) => mark.bits
      case None       => Nil
    }
  }

@main
def tests() = {
  val reg = ("aa") | ("a" ~ (ONE ~ "a"))
  val s   = "aa"
  println(s"1- $s: r=\n${pp(reg)}  ${mat(reg, s)} ")
  println(lexer(reg, s))
  println(rebit.lex(reg, s.toList))
  println("-"*40)

  val reg2 = %("a"|"aa")
  val s2   = "aaa"
  println(s"2- $s2: r=\n${pp(reg2)}  ${mat(reg2, s2)} ")
  println(lexer(reg2, s2))
  println(rebit.lex(reg2, s2.toList))
  println("-"*40)

  val reg3 = (ONE | %("a"))
  val s3   = "a"
  println(s"3- $s3: r=\n${pp(reg3)}  ${mat(reg3, s3)} ")
  println(lexer(reg3, s3))
  println(rebit.lex(reg3, s3.toList))
  println("-"*40)

  val reg4 = ("a"| %("a") )
  val s4   = "a"
  println(s"4- $s4: r=\n${pp(reg4)}  ${mat(reg4, s4)} ")
  println(lexer(reg4, s4))
  println(rebit.lex(reg4, s4.toList))
  println("-"*40)

  val reg5 = ( %(ONE) ~ ("a") )
  val s5   = "a"
  println(s"5- $s5: r=\n${pp(reg5)}  ${mat(reg5, s5)} ")
  println(lexer(reg5, s5))
  println(rebit.lex(reg5, s5.toList))
  println("-"*40)

  val reg6 = ( %("a") | %("aa") )
  val s6   = "aa"
  println(s"6- $s6: r=\n${pp(reg6)}  ${mat(reg6, s6)} ")
  println(lexer(reg6, s6))
  println(rebit.lex(reg6, s6.toList))
  println("-"*40)

  val reg7 = ( ( ONE | "a" ) ~ %("a") )
  val s7   = "a"
  println(s"7- $s7: r=\n${pp(reg7)}  ${mat(reg7, s7)} ")
  println(lexer(reg7, s7))
  println(rebit.lex(reg7, s7.toList))
  println("-"*40)

  val reg8 = ( ONE ~ "a" ) | ("a" ~ ONE ) 
  val s8   = "a"
  println(s"8- $s8: r=\n${pp(reg8)}  ${mat(reg8, s8)} ")
  println(lexer(reg8, s8))
  println(rebit.lex(reg8, s8.toList))
  println("-"*40)

}
@main
def test1() = {
  val reg2 = %("a"|"aa")
  val s2   = "aaa"
  println(s"2- $s2: r=\n${pp(reg2)}  ${mat(reg2, s2)} ")
  println(lexer(reg2, s2))
  println(rebit.lex(reg2, s2.toList))
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
      val vb  = rebit.blex(rebit.intern(r), s.toList)
      val res = vm == vb
      if (!res) {
        println(s"$r and $s")
        println(s"vm=$vm vb=$vb")
        System.exit(1)
      }
    }
  }
}
