error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/shifts_map.sc:contains.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/shifts_map.sc
empty definition using pc, found symbol in pc: contains.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -rexp/ms/contains.
	 -rexp/ms/contains#
	 -rexp/ms/contains().
	 -enumerate/ms/contains.
	 -enumerate/ms/contains#
	 -enumerate/ms/contains().
	 -regenerate/ms/contains.
	 -regenerate/ms/contains#
	 -regenerate/ms/contains().
	 -ms/contains.
	 -ms/contains#
	 -ms/contains().
	 -scala/Predef.ms.contains.
	 -scala/Predef.ms.contains#
	 -scala/Predef.ms.contains().
offset: 4622
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/shifts_map.sc
text:
```scala
import scala.language.implicitConversions
import $file.rexp, rexp._
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
    ms.view.mapValues(p => p.copy(bits = b :: p.bits)).toMap

  def appendBits(bs: Bits): Marks =
    if (bs.isEmpty) ms else ms.view.mapValues(p => p.copy(bits = p.bits ++ bs)).toMap

def mkeps2(r: Rexp): Bits = r match {
  case ONE => Nil
  case ALT(r1, r2) => if (nullable(r1)) Lf :: mkeps2(r1) else Ri :: mkeps2(r2)
  case ALTS(r1, r2, _) => if (nullable(r1)) Lf :: mkeps2(r1) else Ri :: mkeps2(r2)
  case SEQ(r1, r2) => mkeps2(r1) ++ mkeps2(r2)
  case SEQS(r1, r2, _) => mkeps2(r1) ++ mkeps2(r2)
  case STAR(r) => List(En)
  case STARSS(r, _) => List(En)
  case NTIMES(r, _) => List(EnT)
  case AND(r1, r2) => mkeps2(r1) ++ mkeps2(r2)
  case _ => Nil
}

def shifts(ms: Marks, s: String, r: Rexp): Marks =
  ms.valuesIterator.foldLeft(Map.empty[Key, Mark]) { (acc, p) =>
    mergeFirst(acc, shifts1(p, s, r))
  }

def shifts1(p: Mark, s: String, r: Rexp): Marks = r match {
  case ZERO => Map.empty
  case ONE  => Map.empty

  case CHAR(c) =>
    if (p.m < s.length && s(p.m) == c) {
      val q = Mark(p.n, p.m + 1, p.bits)
      Map(key(q) -> q)
    } else Map.empty

  case CHARS(c, _) =>
    if (p.m < s.length && s(p.m) == c) {
      val q = Mark(p.n, p.m + 1, p.bits)
      Map(key(q) -> q)
    } else Map.empty

  case ALT(r1, r2) =>
    val ms0  = Map(key(p) -> p)
    val left  = shifts(ms0, s, r1).prefixBit(Lf)
    val right = shifts(ms0, s, r2).prefixBit(Ri)
    mergeFirst(left, right) 

  case ALTS(r1, r2, _) =>
    val ms0  = Map(key(p) -> p)
    val left  = shifts(ms0, s, r1).prefixBit(Lf)
    val right = shifts(ms0, s, r2).prefixBit(Ri)
    mergeFirst(left, right)

  case SEQ(r1, r2) =>
    val ms0 = Map(key(p) -> p)
    val ms1 = shifts(ms0, s, r1)

    val inR2 =
      if (nullable(r1)) mergeFirst(ms1, ms0) else ms1

    val r1Consume =
      if (nullable(r2)) ms1.appendBits(mkeps2(r2)) else Map.empty

    val r1r2Consume = shifts(inR2, s, r2)

    val r2Consume =
      if (nullable(r1)) shifts(ms0.appendBits(mkeps2(r1)), s, r2) else Map.empty

    (nullable(r1), nullable(r2)) match {
      case (true,  true)  => mergeFirst(mergeFirst(r1Consume, r1r2Consume), r2Consume)
      case (true,  false) => mergeFirst(r1r2Consume, r2Consume)
      case (false, true)  => mergeFirst(r1Consume, r1r2Consume)
      case (false, false) => r1r2Consume
    }

  case SEQS(r1, r2, id) =>
    val ms0 = Map(key(p) -> p)
    val ms1 = shifts(ms0, s, r1)

    val inR2 =
      if (nullable(r1)) mergeFirst(ms1, ms0) else ms1

    val r1Consume =
      if (nullable(r2)) ms1.appendBits(mkeps2(r2)) else Map.empty

    val r1r2Consume = shifts(inR2, s, r2)

    val r2Consume =
      if (nullable(r1)) shifts(ms0.appendBits(mkeps2(r1)), s, r2) else Map.empty

    (nullable(r1), nullable(r2)) match {
      case (true,  true)  => mergeFirst(mergeFirst(r1Consume, r1r2Consume), r2Consume)
      case (true,  false) => mergeFirst(r1r2Consume, r2Consume)
      case (false, true)  => mergeFirst(r1Consume, r1r2Consume)
      case (false, false) => r1r2Consume
    }
  case STARSS(r0, id) =>
    val ms0 = Map(key(p) -> p)
    val ms1 = shifts(ms0.prefixBit(Nx), s, r0)

    if (ms1.isEmpty) Map.empty
    else {
      val outEn  = ms1.prefixBit(En)
      val outRec = shifts(ms1, s, STARSS(r0, id))
      mergeFirst(outEn, outRec)
    }

  case NTIMES(r0, n) =>
    val ms0 = Map(key(p) -> p)

    if (n == 0) Map.empty
    else if (n == 1) {
      val ms1 = shifts(ms0.prefixBit(NxT), s, r0)
      ms1.prefixBit(EnT)
    } else {
      val ms1 = shifts(ms0.prefixBit(NxT), s, r0)
      if (ms1.isEmpty) Map.empty
      else if (nullable(r0)) {
        val outEn  = ms1.prefixBit(EnT)
        val outRec = shifts(ms1, s, NTIMES(r0, n - 1))
        mergeFirst(outEn, outRec)
      } else {
        shifts(ms1, s, NTIMES(r0, n - 1))
      }
    }
}

def mat(r: Rexp, s: String): Marks =
  shifts(Map((0, 0) -> Mark(0, 0, Nil)), s, r)

def lexBits(r: Rexp, s: String): Option[Bits] =
  mat(r, s).get((0, s.length)).map(_.bits)

def matcher(r: Rexp, s: String): Boolean = {
  if (s == "") nullable(r)
  else {
    val ms = mat(r, s)
    ms.c@@ontains((0, s.length))
  }
}

```


#### Short summary: 

empty definition using pc, found symbol in pc: contains.