error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/shifts_map.sc:updated.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/shifts_map.sc
empty definition using pc, found symbol in pc: updated.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -rexp/acc/updated.
	 -rexp/acc/updated#
	 -rexp/acc/updated().
	 -rexps/acc/updated.
	 -rexps/acc/updated#
	 -rexps/acc/updated().
	 -enumerate/acc/updated.
	 -enumerate/acc/updated#
	 -enumerate/acc/updated().
	 -regenerate/acc/updated.
	 -regenerate/acc/updated#
	 -regenerate/acc/updated().
	 -acc/updated.
	 -acc/updated#
	 -acc/updated().
	 -scala/Predef.acc.updated.
	 -scala/Predef.acc.updated#
	 -scala/Predef.acc.updated().
offset: 569
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/shifts_map.sc
text:
```scala
import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.rexps, rexps._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit


case class Mark(n: Int, m: Int, bits: Bits, from: Int, to: Int)

type Key   = (Int, Int)
type Marks = Map[Key, Mark]

def key(p: Mark): Key = (p.n, p.m)

def putFirst(acc: Marks, q: Mark): Marks = {
  val k = key(q)
  if (acc.contains(k)) acc else acc.updated(k, q)
}

def putEarlyFrom(acc: Marks, q: Mark): Marks = {
  val k = key(q)
  acc.get(k) match {
    case None => acc.u@@pdated(k, q)
    case Some(old) =>
      if (q.from < old.from) acc.updated(k, q) else acc
  }
}

def merge(a: Marks, b: Marks): Marks =
  b.valuesIterator.foldLeft(a)(putEarlyFrom)
extension (ms: Marks)
  def prefixBit(b: Bit): Marks =
    ms.view.mapValues(p => p.copy(bits = p.bits :+ b)).toMap

  def appendBits(bs: Bits): Marks =
    if (bs.isEmpty) ms else ms.view.mapValues(p => p.copy(bits = p.bits ++ bs)).toMap

def shifts(ms: Marks, s: String, r: RexpS): Marks =
  ms.valuesIterator.foldLeft(Map.empty[Key, Mark]) { (acc, p) =>
    mergeFirst(acc, shifts1(p, s, r))
  }

def shifts1(p: Mark, s: String, r: RexpS): Marks = {
  val m0 = p.m

  r match {
    case ZEROS => Map.empty
    case ONES  => Map.empty

    case CHARS(c) =>
      if (p.m < s.length && s(p.m) == c) {
        val q = Mark(p.n, p.m + 1, p.bits, from = m0, to = p.m + 1)
        Map(key(q) -> q)
      } else Map.empty

    case ALTS(r1, r2) =>
      val ms0   = Map(key(p) -> p)
      val left  = shifts(ms0, s, r1).prefixBit(Lf)
      val right = shifts(ms0, s, r2).prefixBit(Ri)
      mergeFirst(left, right).view.mapValues(q => q.copy(from = m0, to = q.m)).toMap

    case SEQS(r1, r2, _) =>
      val ms0 = Map(key(p) -> p)
      val ms1 = shifts(ms0, s, r1)

      val n1 = nullableS(r1)
      val n2 = nullableS(r2)

      val inR2 =
        if (n1) mergeFirst(ms1, ms0) else ms1

      val r1Consume =
        if (n2) ms1.appendBits(mkeps2(r2)) else Map.empty

      val r1r2Consume = shifts(inR2, s, r2)

      val r2Consume =
        if (n1) shifts(ms0.appendBits(mkeps2(r1)), s, r2) else Map.empty

      val out =
        (n1, n2) match {
          case (true,  true)  => mergeFirst(mergeFirst(r1Consume, r1r2Consume), r2Consume)
          case (true,  false) => mergeFirst(r1r2Consume, r2Consume)
          case (false, true)  => mergeFirst(r1Consume, r1r2Consume)
          case (false, false) => r1r2Consume
        }

      out.view.mapValues(q => q.copy(from = m0, to = q.m)).toMap

    case STARSS(r0, id) =>
      val ms0 = Map(key(p) -> p)
      val ms1 = shifts(ms0.prefixBit(Nx), s, r0)

      val out =
        if (ms1.isEmpty) Map.empty
        else {
          val outEn  = ms1.prefixBit(En)
          val outRec = shifts(ms1, s, STARSS(r0, id))
          mergeFirst(outEn, outRec)
        }

      out.view.mapValues(q => q.copy(from = m0, to = q.m)).toMap

    case NTIMESS(r0, n) =>
      val ms0 = Map(key(p) -> p)

      val out =
        if (n == 0) Map.empty
        else if (n == 1) {
          val ms1 = shifts(ms0.prefixBit(NxT), s, r0)
          ms1.prefixBit(EnT)
        } else {
          val ms1 = shifts(ms0.prefixBit(NxT), s, r0)
          if (ms1.isEmpty) Map.empty
          else if (nullableS(r0)) {
            val outEn  = ms1.prefixBit(EnT)
            val outRec = shifts(ms1, s, NTIMESS(r0, n - 1))
            mergeFirst(outEn, outRec)
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
  shifts(Map((0, 0) -> Mark(0, 0, Nil, from = 0, to = 0)), s, intern(r))

def matcher(r: Rexp, s: String): Boolean =
  if (s == "") nullable(r)
  else mat(r, s).contains((0, s.length))

def lexer(r: Rexp, s: String): Bits =
  if (s == "") {
    if (nullable(r)) mkeps2(intern(r)) else Nil
  } else {
    val ms = mat(r, s)
    ms.get((0, s.length)) match {
      case Some(mark) => mark.bits
      case None       => Nil
    }
  }



@main
def test1() = {
  val reg = ("aa") | ("a" ~ (ONE ~ "a"))
  val s   = "aa"
  println(s"$s: r=\n${pp(reg)}  ${mat(reg, s)} ")
  println(lexer(reg, s))
  println(rebit.lex(reg, s.toList))
  println("-"*40)

  val reg2 = %("a"|"aa")
  val s2   = "aaa"
  println(s"$s2: r=\n${pp(reg2)}  ${mat(reg2, s2)} ")
  println(lexer(reg2, s2))
  println(rebit.lex(reg2, s2.toList))
}

```


#### Short summary: 

empty definition using pc, found symbol in pc: updated.