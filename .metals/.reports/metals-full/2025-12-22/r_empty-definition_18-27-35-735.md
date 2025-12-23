error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/shifts_map.sc:isEmpty.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/shifts_map.sc
empty definition using pc, found symbol in pc: isEmpty.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -rexp/bs/isEmpty.
	 -rexp/bs/isEmpty#
	 -rexp/bs/isEmpty().
	 -rexps/bs/isEmpty.
	 -rexps/bs/isEmpty#
	 -rexps/bs/isEmpty().
	 -enumerate/bs/isEmpty.
	 -enumerate/bs/isEmpty#
	 -enumerate/bs/isEmpty().
	 -regenerate/bs/isEmpty.
	 -regenerate/bs/isEmpty#
	 -regenerate/bs/isEmpty().
	 -bs/isEmpty.
	 -bs/isEmpty#
	 -bs/isEmpty().
	 -scala/Predef.bs.isEmpty.
	 -scala/Predef.bs.isEmpty#
	 -scala/Predef.bs.isEmpty().
offset: 707
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/shifts_map.sc
text:
```scala
import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.rexps, rexps._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit


case class Mark(n: Int, m: Int, bits: Bits)

type Key   = (Int, Int)          // (n,m)
type Marks = Map[Key, Mark]

def key(p: Mark): Key = (p.n, p.m)
def putFirst(acc: Marks, q: Mark): Marks = {
  val k = key(q)
  if (acc.contains(k)) acc else acc.updated(k, q)
}
def mergeFirst(a: Marks, b: Marks): Marks =
  b.valuesIterator.foldLeft(a)(putFirst)

extension (ms: Marks)
  def prefixBit(b: Bit): Marks =
    ms.view.mapValues(p => p.copy(bits = p.bits :+ b )).toMap
  def appendBits(bs: Bits): Marks =
    if (bs.i@@sEmpty) ms else ms.view.mapValues(p => p.copy(bits = p.bits ++ bs)).toMap


def shifts(ms: Marks, s: String, r: RexpS): Marks =
  ms.valuesIterator.foldLeft(Map.empty[Key, Mark]) { (acc, p) =>
    mergeFirst(acc, shifts1(p, s, r))
  }


def shifts1(p: Mark, s: String, r: RexpS): Marks = r match {
  case ZEROS => Map.empty
  case ONES  => Map.empty

  case CHARS(c) =>
    if (p.m < s.length && s(p.m) == c) {
      val q = Mark(p.n, p.m + 1, p.bits)
      Map(key(q) -> q)
    } else Map.empty


  case ALTS(r1, r2) =>
    val ms0  = Map(key(p) -> p)
    val left  = shifts(ms0, s, r1).prefixBit(Lf)
    val right = shifts(ms0, s, r2).prefixBit(Ri)
    mergeFirst(left, right)


  case SEQS(r1, r2, _) =>
    val ms0 = Map(key(p) -> p)
    val ms1 = shifts(ms0, s, r1)

    val inR2 =
      if (nullableS(r1)) mergeFirst(ms1, ms0) else ms1

    val r1Consume =
      if (nullableS(r2)) ms1.appendBits(mkeps2(r2)) else Map.empty

    val r1r2Consume = shifts(inR2, s, r2)

    val r2Consume =
      if (nullableS(r1))
        shifts(ms0.appendBits(mkeps2(r1)), s, r2)
      else Map.empty

    (nullableS(r1), nullableS(r2)) match {
      case (true,  true)  => mergeFirst(mergeFirst(r1Consume, r1r2Consume), r2Consume)
      case (true,  false) => mergeFirst(r1r2Consume, r2Consume)
      case (false, true)  => mergeFirst(r1Consume, r1r2Consume)
      case (false, false) => r1r2Consume
    }


  case STARSS(r0, _) =>
    val ms0 = Map(key(p) -> p)
    val ms1 = shifts(ms0.prefixBit(Nx), s, r0)

    if (ms1.isEmpty) Map.empty
    else {
      val outEn  = ms1.prefixBit(En)
      val outRec = shifts(ms1, s, STARSS(r0, 0))
      mergeFirst(outEn, outRec)
    }


  case NTIMESS(r0, n) =>
    val ms0 = Map(key(p) -> p)

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


  case ANDS(r1, r2) =>
    val ms0 = Map(key(p) -> p)
    val l = shifts(ms0, s, r1)
    val r = shifts(ms0, s, r2)
    l.keySet.intersect(r.keySet).map(k => k -> l(k)).toMap
}


def mat(r: Rexp, s: String): Marks =
  shifts(Map((0, 0) -> Mark(0, 0, Nil)), s, intern(r))


def matcher(r: Rexp, s: String): Boolean = {
  if (s == "") nullable(r)
  else {
    val ms = mat(r, s)
    ms.contains((0, s.length))
  }
}


def lexer(r: Rexp, s: String): Bits = {
  if (s == "") {
    if (nullable(r)) mkeps2(intern(r)) else Nil
  } else {
    val ms = mat(r, s)
    ms.get((0, s.length)) match {
      case Some(mark) => mark.bits
      case None       => Nil
    }
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

empty definition using pc, found symbol in pc: isEmpty.