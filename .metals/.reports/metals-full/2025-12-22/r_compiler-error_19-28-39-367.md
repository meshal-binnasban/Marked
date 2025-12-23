file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/shifts_map.sc
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 5476
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/DrChristian/shifts_map.sc
text:
```scala
import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.rexps, rexps._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit

case class Mark(n: Int, m: Int, bits: Bits, from: Int, to: Int, k: Int = -1) {
  override def toString: String = {
    val bitsStr = bits.mkString("[", ",", "]")
    s"(n=$n, m=$m, from=$from, to=$to, k=$k, bits=$bitsStr)"
  }
}
type Key   = (Int, Int)
type Marks = Map[Key, Mark]

def key(p: Mark): Key = (p.n, p.m)

def putFirst(acc: Marks, q: Mark): Marks = {
  val kk = key(q)
  if (acc.contains(kk)) acc else acc.updated(kk, q)
}

// Prefer larger k; if k ties, prefer earlier from; otherwise keep old.
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
}

def merge(a: Marks, b: Marks): Marks =
  b.valuesIterator.foldLeft(a)(putPrefer)

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
        val q = Mark(p.n, p.m + 1, p.bits, from = m0, to = p.m + 1, k = p.k)
        Map(key(q) -> q)
      } else Map.empty

    case ALTS(r1, r2) =>
        val ms0   = Map(key(p) -> p)
        val left  = shifts(ms0.prefixBit(Lf), s, r1)
        val right = shifts(ms0.prefixBit(Ri), s, r2)
        merge(left, right).view.mapValues(q => q.copy(from = m0, to = q.m)).toMap

    case SEQS(r1, r2, _) =>
      val ms0 = Map(key(p) -> p)
      val ms1 = shifts(ms0, s, r1)

      val n1 = nullableS(r1)
      val n2 = nullableS(r2)

      val inR2 =
        if (n1) merge(ms1, ms0) else ms1

      val r1Consume =
        if (n2) ms1.appendBits(mkeps2(r2)) else Map.empty

      val r1r2Consume = shifts(inR2, s, r2)

      val r2Consume =
        if (n1) shifts(ms0.appendBits(mkeps2(r1)), s, r2) else Map.empty

      val out =
        (n1, n2) match {
          case (true,  true)  => merge(merge(r1Consume, r1r2Consume), r2Consume)
          case (true,  false) => merge(r1r2Consume, r2Consume)
          case (false, true)  => merge(r1Consume, r1r2Consume)
          case (false, false) => r1r2Consume
        }

      out.view.mapValues(q => q.copy(from = m0, to = q.m)).toMap

    // Key idea: set k ONCE to the first-iteration boundary and carry it.
    case STARSS(r0, id) =>
      val ms0    = Map(key(p) -> p)
      val ms1raw = shifts(ms0.prefixBit(Nx), s, r0)

      // marks produced by the *first* iteration: if k unset, set k = their end position
      val ms1 = ms1raw.view.mapValues(q => if (q.k == -1) q.copy(k = q.m) else q).toMap

      val out =
        if (ms1.isEmpty) Map.empty
        else {
          val outEn  = ms1.prefixBit(En)
          val outRec = shifts(ms1, s, STARSS(r0, id))
          merge(outEn, outRec)
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
  shifts(Map((0, 0) -> Mark(0, 0, Nil, from = 0, to = 0, k = -1)), s, intern(r))

def matcher(r: Rexp, s: String): Boolean =
  if (s == "") nullable(r) else mat(r, s).contains((0, s.length))

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
  println("-"*40)

  val reg3 = (ONE | %("a"))
  val s3   = "a"
  println(s"$s3: r=\n${pp(reg3)}  ${mat(reg3, s3)} ")
  println(lexer(reg3, s3))
  println(rebit.lex(reg3, s3.toList))
  println("-"*40)

  val reg4 = ("a"| (@@)"a") )
  val s4   = "aaa"
  println(s"$s4: r=\n${pp(reg4)}  ${mat(reg4, s4)} ")
  println(lexer(reg4, s4))
  println(rebit.lex(reg4, s4.toList))
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

```



#### Error stacktrace:

```
scala.collection.LinearSeqOps.apply(LinearSeq.scala:129)
	scala.collection.LinearSeqOps.apply$(LinearSeq.scala:128)
	scala.collection.immutable.List.apply(List.scala:79)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:244)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:101)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:88)
	dotty.tools.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:46)
	dotty.tools.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:435)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: -1